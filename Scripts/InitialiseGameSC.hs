module  InitialiseGameSC    ( typedValidator
                            , validator
                            , script
                            , scriptHash
                            , scriptCredential
                            , address
                            ) where

import  Plutarch.Prelude
import  Plutarch.Num                ( (#+)   )
import  Plutarch.Script             ( Script )
import  Plutarch.Monadic            qualified as P
import  Plutarch.Api.V1.Address     ( PCredential )
import  Plutarch.Api.V2             ( PValidator, PScriptHash, PStakingCredential, PAddress                                    
                                    , PPOSIXTime, PExtended (..)
                                    , PMaybeData (..)
                                    )
import  Plutarch.Api.V2.Contexts    ( PScriptPurpose(PSpending), PScriptContext )
import  Plutarch.Extra.Field        ( pletAll )
import  PDataTypes                  ( PInitialization(..) , PGameSettings , PGameState(PGame) , PGameInfo(..) , PPlayer(PBluePlayer) )
import  PUtilities
import  RunGameSC                   qualified

----------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------ | Validator | -------------------------------------------------------

typedValidator :: Term s (PGameSettings :--> PInitialization :--> PScriptContext :--> PBool)
typedValidator = phoistAcyclic $ plam $ \ gameSettings initialization scriptContext ->
    pmatch initialization $ \case
    {- Start the Game -}
        PAdd ( pfromData . (pfield @"player" #) -> playerB) -> P.do
            scriptContext <- pletAll scriptContext
            txInfo <- pletFields @'["inputs", "outputs", "validRange"] scriptContext.txInfo
            let ownTxOutRef = pmatch scriptContext.purpose $ \case  PSpending txOutRef -> pfield @"_0" # txOutRef
                                                                    _ -> ptraceError "ScriptPurpose is not PSpending! @validator"
            let betAmount = getInputValue # ownTxOutRef # txInfo.inputs

                currentTime = pmatch (pfield @"_0" #$ pfield @"from" # txInfo.validRange)
                            $ \case PFinite a -> pfield @"_0" # a
                                    _ -> ptraceError "Invalid LowerBound POSIXTime! @validator"
        -- Calculated variables
            let cGameInfo = calculateGameInfo gameSettings playerB currentTime
                cDatum    = toPOutputDatum cGameInfo
                cTotalBet = betAmount <> betAmount
        -- Proposed variables
            outputUTxO <- pletFields @'["value", "datum"] $ getOutputUTxO # RunGameSC.address # txInfo.outputs

            pAnd'   [ outputUTxO.value #== cTotalBet
                    , outputUTxO.datum #== cDatum
                    ]

            -- 1 - UTxO going to runGameSC
            -- 2 - Value doubled
            -- 3 - correct Datum:
                -- a - right players -- ! Need to check that players are of different colors !
                -- b - same turn duration
                -- c - same board

    {- Cancel the Game -}
        PWithdraw _ -> P.do
            PBluePlayer nft <- pmatch $ pfield @"player1" # gameSettings
            nft <- pletAll nft
            elemNFT # nft.symbol # nft.name #$ pfield @"inputs" #$ pfield @"txInfo" # scriptContext

--------------------------------------------------- | Helper Functions | ---------------------------------------------------

calculateGameInfo :: Term s PGameSettings -> Term s PPlayer -> Term s PPOSIXTime -> Term s PGameInfo
calculateGameInfo settings playerB currentTime = P.do
    settings <- pletAll settings
    turnDuration        <- plet settings.turnDuration
    let playerA         = settings.player1
        boardS0         = settings.boardS0

        players         = pcons # playerA #$ pcons # playerB # pnil
        deadline        = currentTime #+ turnDuration
        gameState       = pcon . PGame   $ pdcons @"player'sTurn"   # pdata playerB
                                        #$ pdcons @"deadline"       # pdata deadline
                                        #$ pdcons @"board"          # boardS0
                                        #  pdnil

        gameInfo        = pcon . PGameInfo   $ pdcons @"players"        # pdata players
                                            #$ pdcons @"turnDuration"   # pdata turnDuration
                                            #$ pdcons @"gameState"      # pdata gameState
                                            #  pdnil
    gameInfo

---------------------------------------------------- | Serializations | ----------------------------------------------------

validator :: Term s PValidator
validator = pwrapValidator # typedValidator

script :: Script
script = validatorToScript validator

scriptHash :: Term s PScriptHash
scriptHash = hashScript script

scriptCredential :: Term s PCredential
scriptCredential = mkScriptCredential scriptHash

address :: Term s PAddress
address = mkAddress scriptCredential stakingCredential
    where
        stakingCredential :: Term s (PMaybeData PStakingCredential)
        stakingCredential = pcon $ PDNothing pdnil

------------------------------------------------------- End Of Code --------------------------------------------------------