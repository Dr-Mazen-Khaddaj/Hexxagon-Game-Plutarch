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
import  PDataTypes
import  PUtilities
import  RunGameSC                   qualified

----------------------------------------------------------------------------------------------------------------------------
{-  Conditions (Start Game) :
        1- Only 1 UTxO consumed from InitialiseGameSC.
        2- New UTxO is created at RunGameSC => That has it's:
        3- Value doubled
        4- correct Datum:
            a - right players
            b - same turn duration
            c - right player's turn (PlayerB)
            d - correct Deadline
            e - same board
        5- Legal Redeemer (PlayerB):
            a - Color Red
            b - Registering a unique NFT i.e different from PlayerA's NFT.

    Conditions (Cancel Game) :
        The registered NFT is found in the input UTxOs.
-}
------------------------------------------------------ | Validator | -------------------------------------------------------

typedValidator :: Term s (PGameSettings :--> PInitialization :--> PScriptContext :--> PBool)
typedValidator = phoistAcyclic $ plam $ \ gameSettings initialization scriptContext ->
    pmatch initialization $ \case
    {- Start the Game -}
        PAdd ( pfromData . (pfield @"player" #) -> playerB) -> P.do
            scriptContext <- pletAll scriptContext
            txInfo <- pletFields @'["inputs", "outputs", "validRange"] scriptContext.txInfo
            let ownTxOutRef = pmatch scriptContext.purpose
                            $ \case PSpending txOutRef  -> pfield @"_0" # txOutRef
                                    _                   -> ptraceError "ScriptPurpose is not PSpending! @typedValidator"

                ownCredential = pfield @"credential" #$ getOwnAddress # ownTxOutRef # txInfo.inputs
                betAmount = getInputValue # ownCredential # txInfo.inputs                                               -- C1

                currentTime = pmatch (pfield @"_0" #$ pfield @"from" # txInfo.validRange)
                            $ \case PFinite a   -> pfield @"_0" # a
                                    _           -> ptraceError "Invalid LowerBound POSIXTime! @typedValidator"

        -- Calculated variables
            PBluePlayer playerA'sNFT <- pmatch $ pfield @"player1" # gameSettings
            PRedPlayer  playerB'sNFT <- pmatch playerB                                                                  -- C5.a
            let cGameInfo = calculateGameInfo gameSettings playerB currentTime
                cDatum    = toPOutputDatum cGameInfo
                cTotalBet = betAmount <> betAmount

        -- Proposed variables
            outputUTxO <- pletFields @'["value", "datum"] $ getOutputUTxO # RunGameSC.address # txInfo.outputs          -- C2

            pAnd'   [ outputUTxO.value #== cTotalBet                                                                    -- C3
                    , outputUTxO.datum #== cDatum                                                                       -- C4
                    , pnot #$ playerA'sNFT #== playerB'sNFT                                                             -- C5.b
                    ]

    {- Cancel the Game -}
        PWithdraw _ -> P.do
            PBluePlayer nft <- pmatch $ pfield @"player1" # gameSettings
            nft <- pletAll nft
            elemNFT # nft.symbol # nft.name #$ pfield @"inputs" #$ pfield @"txInfo" # scriptContext

--------------------------------------------------- | Helper Functions | ---------------------------------------------------

calculateGameInfo :: Term s PGameSettings -> Term s PPlayer -> Term s PPOSIXTime -> Term s PGameInfo
calculateGameInfo settings playerB currentTime = P.do
    settings        <- pletAll settings
    turnDuration    <- plet settings.turnDuration                                                                       -- C4.b
    let playerA     = settings.player1
        players     = pcons # playerA #$ pcons # playerB # pnil                                                         -- C4.a
        deadline    = currentTime #+ turnDuration                                                                       -- C4.d
        gameState   = pcon . PGame   $ pdcons @"player'sTurn"   # pdata playerB                                         -- C4.c
                                    #$ pdcons @"deadline"       # pdata deadline
                                    #$ pdcons @"board"          # settings.boardS0                                      -- C4.e
                                    #  pdnil

        gameInfo    = pcon . PGameInfo   $ pdcons @"players"        # pdata players
                                        #$ pdcons @"turnDuration"   # pdata turnDuration
                                        #$ pdcons @"gameState"      # pdata gameState
                                        #  pdnil
    gameInfo

---------------------------------------------------- | Serializations | ----------------------------------------------------

validator :: Term s PValidator
validator = pwrapValidator # typedValidator

script :: Script
script = closedTermToScript validator

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