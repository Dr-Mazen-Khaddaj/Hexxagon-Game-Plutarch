module  InitialiseGameSC    ( typedValidator
                            , validator
                            , script
                            , scriptHash
                            , scriptCredential
                            , address
                            ) where

import  Plutarch.Prelude
import  Plutarch.Num                ( (#+)          )
import  Plutarch.Unsafe             ( punsafeCoerce )
import  Plutarch.Bool               ( pand'         )
import  Plutarch.Script             ( Script        )
import  Plutarch.Monadic            qualified as P
import  Plutarch.Api.V1.Address     ( PCredential   )
import  Plutarch.Api.V1.AssocMap    ( plookup       )
import  Plutarch.Api.V2             ( PValidator, PScriptHash, PStakingCredential, PAddress
                                    , PValue (..), KeyGuarantees (..), AmountGuarantees (..)
                                    , PPOSIXTime, PExtended (..)
                                    , PDatum (..), PMaybeData (..)
                                    , PCurrencySymbol, PTokenName
                                    )
import  Plutarch.Api.V2.Tx          ( PTxOut, PTxOutRef, PTxInInfo, POutputDatum (..) )
import  Plutarch.Api.V2.Contexts    ( PScriptPurpose(PSpending), PScriptContext )
import  Plutarch.Extra.Field        ( pletAll )
import  PDataTypes                  ( PInitialization(..) , PGameSettings , PGameState(PGame) , PGameInfo(..) , PPlayer(PBluePlayer) )
import  PUtilities                  ( mkAddress, mkScriptCredential, hashScript, validatorToScript, pwrapValidator )
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
            let pValue = outputUTxO.value
                pDatum = outputUTxO.datum

            pand'   # (pValue #== cTotalBet)
                    # (pDatum #== cDatum)

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

getOutputUTxO :: Term s (PAddress :--> PBuiltinList PTxOut :--> PTxOut)
getOutputUTxO = phoistAcyclic $ plam $ \ addr txOuts ->
    precList    (\self txOut rest ->    let txOutAddr = pfield @"address" # txOut
                                        in  pif (txOutAddr #== addr)
                                                (txOut)
                                                (self # rest) )
                (\_ -> ptraceError "Can't find UTxO @getOutputUTxO")
                # txOuts

---------------------------------------------------

getInputValue :: Term s (PTxOutRef :--> PBuiltinList PTxInInfo :--> PValue 'Sorted 'Positive)
getInputValue = phoistAcyclic $ plam $ \ txOutRef txIns ->
    precList    (\self txIn rest -> let txInRef = pfield @"outRef" # txIn
                                    in  pif (txInRef #== txOutRef)
                                            (precList   (\self' txIn' rest' ->  let txInRef' = pfield @"outRef" # txIn'
                                                                                in  pif (txInRef' #== txOutRef)
                                                                                        (ptraceError "Found more than 1 UTxO! @getInputUTxO")
                                                                                        (self' # rest') )
                                                        (\_ -> pfield @"value" #$ pfield @"resolved" # txIn)
                                                        # rest )
                                            (self # rest) )
                (\_ -> ptraceError "Can't find UTxO! @getInputUTxO")
                # txIns

---------------------------------------------------

calculateGameInfo :: Term s PGameSettings -> Term s PPlayer -> Term s PPOSIXTime -> Term s PGameInfo
calculateGameInfo settings playerB currentTime = P.do
    settings <- pletAll settings
    turnDuration        <- plet settings.turnDuration
    let playerA         = settings.player1
        boardS0         = settings.boardS0

        players         = pcon . PCons playerA $ pcon . PCons playerB $ pcon PNil
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

---------------------------------------------------

toPOutputDatum :: Term s a -> Term s POutputDatum
toPOutputDatum d = pcon . POutputDatum $ pdcons @"outputDatum" # (pdata . pcon . PDatum $ punsafeCoerce d) # pdnil

---------------------------------------------------

elemNFT :: Term s (PCurrencySymbol :--> PTokenName :--> PBuiltinList PTxInInfo :--> PBool)
elemNFT = phoistAcyclic $ plam $ \ symbol name inputs ->
    precList
        (\onInputs input restOfInputs -> P.do
            PValue valueMap <- pmatch $ pfield @"value" #$ pfield @"resolved" # input
            pmatch (plookup # symbol # valueMap)
                (\case  PJust tokens -> pmatch (plookup # name # tokens)
                                            (\case  PJust _  -> pcon PTrue
                                                    PNothing -> onInputs # restOfInputs )
                        PNothing -> onInputs # restOfInputs ))
        (\_ -> pcon PFalse)
        # inputs

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