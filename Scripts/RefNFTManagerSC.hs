module  RefNFTManagerSC ( typedValidator
                        , validator
                        , script
                        , scriptHash
                        , scriptCredential
                        , address
                        ) where

import  Plutarch
import  Plutarch.Prelude
import  Plutarch.Api.V2             hiding (scriptHash)
import  Plutarch.Api.V1.AssocMap    ( plookup, pinsert, passertSorted, pforgetSorted )
import  Plutarch.Api.V1.Address     ( PCredential   )
import  Plutarch.Maybe              ( pfromJust     )
import  Plutarch.Unsafe             ( punsafeCoerce )
import  Plutarch.Num                ( (#+)          )
import  Plutarch.Extra.Field        ( pletAll       )
import  PUtilities
import  PDataTypes                  ( PPlayer (..), PMetadata (..) )
import  RunGameSC                   qualified
import  Plutarch.Monadic            qualified as P

----------------------------------------------------------------------------------------------------------------------------
{-  Conditions :
    (Right to update Metadata)
        1 - Own Input (RefNFTManagerSC) is valid: Value Contains the right RefNFT
            (calculated from the exposed winner's registered NFT @ RunGameSC Input's Redeemer).
    (Metadata updated correctly)
        2 - Output to RefNFTManagerSC has:
            a - Value preserved (2ADA + RefNFT)
            b - Datum is the newly calculated Metadata
-}
----------------------------------------------------------------------------------------------------------------------------

typedValidator :: Term s (PMetadata :--> PUnit :--> PScriptContext :--> PBool)
typedValidator = phoistAcyclic $ plam $ \ metadata _ scriptContext -> P.do
    scriptContext <- pletAll scriptContext
    txInfo <- pletFields @'["inputs", "outputs","redeemers"] scriptContext.txInfo

-------------------------------------------------------------- from RunGameSC
-- Info from RunGameSC
    let inputTxOutRefFromRunGameSC = getInputUTxORef # RunGameSC.scriptCredential # txInfo.inputs
        scriptPurposeFromRunGameSC = pcon . PSpending $ pdcons # (pdata $ inputTxOutRefFromRunGameSC) # pdnil
        winningPlayer = punsafeCoerce @_ @_ @PPlayer $ pfromJust #$ plookup # scriptPurposeFromRunGameSC # txInfo.redeemers
    winnerNFT <- pletAll $ pmatch winningPlayer (\case PBluePlayer nft -> nft ; PRedPlayer nft -> nft)
    PTokenName winnerNFTName <- pmatch winnerNFT.name
-- Calculated variables from RunGameSC
    let refNFTLabel     = phexByteStr "000643b0"
        refNFTName      = pcon . PTokenName $ refNFTLabel <> psliceBS # 4 # 28 # winnerNFTName
        refNFT          = toPBuiltinPair winnerNFT.symbol  (pcon . PMap $ pcons # toPBuiltinPair refNFTName 1 # pnil)
        cRefNFTWith2ADA = pcon . PValue $ pcon . PMap $ pcons # q2ADA #$ pcons # refNFT # pnil
-------------------------------------------------------------- from RefNFTManagerSC
-- Info from RefNFTManagerSC
    let ownTxOutRef = pmatch scriptContext.purpose
                    $ \case PSpending txOutRef  -> pfield @"_0" # txOutRef
                            _                   -> ptraceError "ScriptPurpose is not PSpending! @typedValidator"
    ownAddress <- plet $ getOwnAddress # ownTxOutRef # txInfo.inputs
    let ownCredential = pfield @"credential" #$ ownAddress
        ownValue = getInputValue # ownCredential # txInfo.inputs

    metadata <- pletAll metadata
    let score = pmatch (plookup # pconstant "score" # metadata.metadata)
            $ \ case PJust a -> a ; PNothing -> ptraceError "Can't find score @typedValidator"
-- Calculated variables from RefNFTManagerSC
        newScore = punsafeCoerce @_ @_ @PData $ (punsafeCoerce @_ @_ @PInteger score) #+ 1
        newMetadata = pcon . PMetadata  $  pdcons # (pdata . pforgetSorted $ pinsert # pconstant "score" # newScore #$ passertSorted # metadata.metadata)
                                        #$ pdcons # metadata.versionNum
                                        #$ pdcons # metadata.extraData
                                        #  pdnil
        cRefNFTOutputDatum = toPOutputDatum newMetadata
-- Proposed variables to RefNFTManagerSC
    ownOutput <- pletFields @'["value", "datum"] $ getOutputUTxO # ownAddress # txInfo.outputs

    pAnd'   [ ownValue #== cRefNFTWith2ADA              -- C1
            , ownOutput.value #== ownValue              -- C2a
            , ownOutput.datum #== cRefNFTOutputDatum    -- C2b
            ]

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