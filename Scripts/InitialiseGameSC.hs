module InitialiseGameSC (validator) where

import Plutarch.Prelude
import PDataTypes
import Plutarch.Api.V2.Contexts
import Plutarch.Api.V2.Tx (PTxOut, PTxOutRef, PTxInInfo)
import Plutarch.Api.V2 (PAddress, PValue, KeyGuarantees (Sorted), AmountGuarantees (Positive), PPOSIXTime, PExtended (PFinite))
import Plutarch.Extra.Field (pletAll)
import qualified Plutarch.Monadic as P
import Plutarch.Num ((#+))
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Bool (pand')

----------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------ | Validator | -------------------------------------------------------

validator :: Term s (PGameSettings :--> PInitialization :--> PScriptContext :--> PBool)
validator = phoistAcyclic $ plam $ \ gameSettings initialization scriptContext ->
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
                cDatum    = punsafeCoerce cGameInfo
                cTotalBet = betAmount <> betAmount
        -- Proposed variables
            let runGameSCAddr = undefined
            outputUTxO <- pletFields @'["value", "datum"] $ getOutputUTxO # runGameSCAddr # txInfo.outputs
            let pValue = outputUTxO.value
                pDatum = outputUTxO.datum

            pand'   # (pValue #== cTotalBet)
                    # (pDatum #== cDatum)

            -- 1 - UTxO going to runGameSC
            -- 2 - Value doubled
            -- 3 - correct Datum:
                -- a - right players
                -- b - same turn duration
                -- c - same board

    {- Cancel the Game -}
        PWithdraw _ -> undefined

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

----------------------------------------------------- | Benchmarking | -----------------------------------------------------

------------------------------------------------------- End Of Code --------------------------------------------------------