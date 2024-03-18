module  PlayerIdentifierMP  ( typedMintingPolicy
                            , script
                            , scriptHash
                            ) where

import  Plutarch
import  Plutarch.Prelude
import  Plutarch.Api.V1.Value   (padaToken, padaSymbol)
import  Plutarch.Api.V1.Tuple   (pbuiltinPairFromTuple, ptuple)
import  Plutarch.Api.V2         hiding (scriptHash)
import  Plutarch.Extra.Field    (pletAll)
import  PlutusLedgerApi.V2      (BuiltinData(..), Data (..))
import  UtilityFxs              (bytesFromHex)
import  DataTypes               (Metadata(..))
import  PUtilities
import  PDataTypes              ()
import  Plutarch.Monadic        qualified as P
import  PlutusTx.AssocMap       qualified as AssocMap
import  RefNFTManagerSC         qualified

----------------------------------------------------------------------------------------------------------------------------
{-  Conditions :
        1 - Unique TxOutRef is found in the inputs.
        2 - Minted Tokens are valid according to cip-68 (symbol & name).
        3 - Minted refNFT is sent to RefNFTManagerSC.
        4 - Datum of TxOut at RefNFTManagerSC (Single output that holds the refNFT) is correct.
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

    let refNFT = toPBuiltinPair ownCurrencySymbol (pcon . PMap $ pcons # toPBuiltinPair refNFT_Name 1 # pnil)
        q2ADA  = toPBuiltinPair padaSymbol        (pcon . PMap $ pcons # toPBuiltinPair padaToken 2_000_000 # pnil)
        q0ADA  = toPBuiltinPair padaSymbol        (pcon . PMap $ pcons # toPBuiltinPair padaToken 0         # pnil)

    let tokens = pcon . PMap $ pcons # toPBuiltinPair refNFT_Name  1
                            #$ pcons # toPBuiltinPair userNFT_Name 1 # pnil
        cMintedValue = pcon . PValue $ pcon . PMap  $  pcons # q0ADA
                                                    #$ pcons # toPBuiltinPair ownCurrencySymbol tokens # pnil

    let cRefNFTWith2ADA = pcon . PValue $ pcon . PMap $ pcons # q2ADA #$ pcons # refNFT # pnil

    let cMetadata = Metadata hexxagonMetadata 2 (BuiltinData $ List [])
        hexxagonMetadata = AssocMap.fromList    [ ("name"           , "Hexxagon Game"                                           )
                                                , ("image"          , "ipfs://QmcNdgGVQ5Yw9ckWH2PohYKyvmud2MaksyzHz1SBAoM89h"   )
                                                , ("description"    , "Hexxagon game on the Cardano Blockchain"                 )
                                                , ("authors"        , "Dr. Mazen Khaddaj & Andrew Garrett Wright"               )
                                                , ("score"          , "0"                                                       )
                                                ]
        cRefNFTOutputDatum = toPOutputDatum $ pconstant cMetadata

    -- Proposed variables
    refNFTOutputUTxO <- pletFields @'["value", "datum"] $ getOutputUTxO # RefNFTManagerSC.address # txInfo.outputs

    pAnd'   [ nonFungible                                       -- C1
            , txInfo.mint #== cMintedValue                      -- C2
            , refNFTOutputUTxO.value #== cRefNFTWith2ADA        -- C3
            , refNFTOutputUTxO.datum #== cRefNFTOutputDatum     -- C4
            ]

toPBuiltinPair :: forall {a :: PType} {b :: PType} {s :: S}. (PIsData a, PIsData b) => Term s a -> Term s b -> Term s (PBuiltinPair (PAsData a) (PAsData b))
toPBuiltinPair a b = pfromData $ pbuiltinPairFromTuple $ pdata $ ptuple # pdata a # pdata b

---------------------------------------------------- | Serializations | ----------------------------------------------------

mintingPolicy :: Term s (PTxOutRef :--> PMintingPolicy)
mintingPolicy = phoistAcyclic $ plam $ \ txOutRef -> pwrapPolicy #$ typedMintingPolicy # txOutRef

script :: Script
script = closedTermToScript mintingPolicy

scriptHash :: Term s PScriptHash
scriptHash = hashScript script

------------------------------------------------------- End Of Code --------------------------------------------------------