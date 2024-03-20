module  PlayerIdentifierMP  ( typedMintingPolicy
                            , script
                            , scriptHash
                            ) where

import  Plutarch
import  Plutarch.Prelude
import  Plutarch.Api.V2         hiding (scriptHash)
import  Plutarch.Extra.Field    (pletAll)
import  UtilityFxs              (bytesFromHex)
import  DataTypes               (Metadata(..))
import  PUtilities
import  PDataTypes              ()
import  Plutarch.Monadic        qualified as P
import  PlutusCore.Data         (Data(..))
import  PlutusTx.AssocMap       qualified as AssocMap
import  RefNFTManagerSC         qualified

----------------------------------------------------------------------------------------------------------------------------
{-  Conditions :
        1 - Unique TxOutRef is found in the inputs.
        2 - Minted Tokens are valid according to cip-68 (symbol & name).
        3 - Minted refNFT is sent to RefNFTManagerSC.
        4 - Datum of TxOut at RefNFTManagerSC (That holds the refNFT) is correct.
-}
----------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------- | Minting Policy | ----------------------------------------------------

typedMintingPolicy :: Term s (PTxOutRef :--> PUnit :--> PScriptContext :--> PBool)
typedMintingPolicy = phoistAcyclic $ plam $ \ uniqueRef _ scriptContext -> P.do
    scriptContext   <- pletAll scriptContext
    txInfo          <- pletFields @'["inputs","outputs","mint"] scriptContext.txInfo
    PMinting ((pfield @"_0" #) -> ownCurrencySymbol) <- pmatch scriptContext.purpose

    let nonFungible = precList
            (\self input restOfInputs -> pif    (pfield @"outRef" # input #== uniqueRef)
                                                (pconstant True)
                                                (self # restOfInputs) )
            (\_ -> pconstant False)
            # pfromData txInfo.inputs

    -- Calculated variables
    let userNFTLabel = bytesFromHex "000de140"
    let refNFTLabel  = bytesFromHex "000643b0"

    let userNFT_Name    = pcon $ PTokenName $ pconstant $ userNFTLabel <> "HEXXAGON"
    refNFT_Name <- plet $ pcon $ PTokenName $ pconstant $ refNFTLabel  <> "HEXXAGON"

    let tokens = pcon . PMap $ pcons # toPBuiltinPair refNFT_Name  1
                            #$ pcons # toPBuiltinPair userNFT_Name 1 # pnil
        cMintedValue = pcon . PValue $ pcon . PMap  $  pcons # q0ADA
                                                    #$ pcons # toPBuiltinPair ownCurrencySymbol tokens # pnil

    let refNFT = toPBuiltinPair ownCurrencySymbol (pcon . PMap $ pcons # toPBuiltinPair refNFT_Name 1 # pnil)
        cRefNFTWith2ADA = pcon . PValue $ pcon . PMap $ pcons # q2ADA #$ pcons # refNFT # pnil

    let cMetadata = Metadata hexxagonMetadata 2 (List [])
        hexxagonMetadata = AssocMap.fromList    [ ("name"           , B "Hexxagon Game"                                          )
                                                , ("image"          , B "ipfs://QmcNdgGVQ5Yw9ckWH2PohYKyvmud2MaksyzHz1SBAoM89h"  )
                                                , ("description"    , B "Hexxagon game on the Cardano Blockchain"                )
                                                , ("authors"        , B "Dr. Mazen Khaddaj & Andrew Garrett Wright"              )
                                                , ("score"          , I 0                                                        )
                                                ]
        cRefNFTOutputDatum = toPOutputDatum $ pconstant cMetadata

    -- Proposed variables
    refNFTOutputUTxO <- pletFields @'["value", "datum"] $ getOutputUTxO # RefNFTManagerSC.address # txInfo.outputs

    pAnd'   [ nonFungible                                       -- C1
            , txInfo.mint #== cMintedValue                      -- C2
            , refNFTOutputUTxO.value #== cRefNFTWith2ADA        -- C3
            , refNFTOutputUTxO.datum #== cRefNFTOutputDatum     -- C4
            ]

---------------------------------------------------- | Serializations | ----------------------------------------------------

mintingPolicy :: Term s (PTxOutRef :--> PMintingPolicy)
mintingPolicy = phoistAcyclic $ plam $ \ txOutRef -> pwrapPolicy #$ typedMintingPolicy # txOutRef

script :: Script
script = closedTermToScript mintingPolicy

scriptHash :: Term s PScriptHash
scriptHash = hashScript script

------------------------------------------------------- End Of Code --------------------------------------------------------