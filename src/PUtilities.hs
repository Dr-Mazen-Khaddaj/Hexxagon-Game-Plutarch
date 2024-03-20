module  PUtilities  ( pwrapValidator
                    , pwrapPolicy
                    , closedTermToScript
                    , hashScript
                    , mkScriptCredential
                    , mkAddress
                    , writeScriptToFile
                    , pAnd'
                    , toPOutputDatum
                    , toPBuiltinPair
                    , elemNFT
                    , getOwnAddress
                    , getInputValue
                    , getInputUTxO
                    , getInputUTxORef
                    , getOutputUTxO
                    , q0ADA
                    , q2ADA
                    ) where

import  Plutarch.Prelude
import  Plutarch
import  Plutarch.Monadic            qualified as P
import  Plutarch.Bool               ( pand' )
import  Plutarch.Api.V1             ( PCredential (..) )
import  Plutarch.Api.V1.AssocMap    ( plookup )
import  Plutarch.Api.V1.Tuple       ( pbuiltinPairFromTuple, ptuple )
import  Plutarch.Api.V1.Value       ( padaSymbol, padaToken )
import  Plutarch.Api.V2
import  Plutarch.Unsafe             ( punsafeCoerce   )
import  Plutarch.Script             ( serialiseScript )
import  Data.Aeson                  ( object, (.=) )
import  Data.Aeson.Encode.Pretty    ( encodePretty )
import  Data.Text                   ( Text )
import  Data.Text.Encoding          qualified as Text
import  Data.ByteString.Lazy        qualified as LBS
import  Data.ByteString.Base16      qualified as B16
import  Data.ByteString.Short       qualified as SBS

--------------------------------------------------- | Helper Functions | ---------------------------------------------------

pAnd' :: Foldable t => t (Term s PBool) -> Term s PBool
pAnd' = foldr ((#) . (pand' #)) (pconstant True)

---------------------------------------------------

toPOutputDatum :: Term s a -> Term s POutputDatum
toPOutputDatum d = pcon . POutputDatum $ pdcons @"outputDatum" # (pdata . pcon . PDatum $ punsafeCoerce d) # pdnil

---------------------------------------------------

toPBuiltinPair :: forall {a :: PType} {b :: PType} {s :: S}. (PIsData a, PIsData b) => Term s a -> Term s b -> Term s (PBuiltinPair (PAsData a) (PAsData b))
toPBuiltinPair a b = pfromData $ pbuiltinPairFromTuple $ pdata $ ptuple # pdata a # pdata b

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

---------------------------------------------------

q0ADA, q2ADA :: forall {s :: S} {keysort :: KeyGuarantees}. Term s (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap keysort PTokenName PInteger)))
q0ADA = toPBuiltinPair padaSymbol (pcon . PMap $ pcons # toPBuiltinPair padaToken 0         # pnil)
q2ADA = toPBuiltinPair padaSymbol (pcon . PMap $ pcons # toPBuiltinPair padaToken 2_000_000 # pnil)

---------------------------------------------------

getOwnAddress :: Term s (PTxOutRef :--> PBuiltinList PTxInInfo :--> PAddress)
getOwnAddress = phoistAcyclic $ plam $ \ ownRef txIns ->
    precList    (\self txIn rest -> let txOutRef = pfield @"outRef" # txIn
                                    in  pif (txOutRef #== ownRef)
                                            (pfield @"address" #$ pfield @"resolved" # txIn)
                                            (self # rest)
                )
                (\_ -> ptraceError "Can't find own TxOutRef! @getOwnAddress")
                # txIns

---------------------------------------------------

getInputValue :: Term s (PCredential :--> PBuiltinList PTxInInfo :--> PValue 'Sorted 'Positive)
getInputValue = phoistAcyclic $ plam $ \ credential txIns ->
    precList    (\self txIn rest -> let getCredential txInInfo = pfromData $ pfield @"credential" #$ pfield @"address" #$ pfield @"resolved" # txInInfo
                                    in  pif (getCredential txIn #== credential)
                                            (precList   (\self' txIn' rest' -> pif  (getCredential txIn' #== credential)
                                                                                    (ptraceError "Found more than 1 UTxO! @getInputValue")
                                                                                    (self' # rest') )
                                                        (\_ -> pfield @"value" #$ pfield @"resolved" # txIn)
                                                        # rest )
                                            (self # rest) )
                (\_ -> ptraceError "Can't find UTxO! @getInputValue")
                # txIns

---------------------------------------------------

getInputUTxO :: Term s (PCredential :--> PBuiltinList PTxInInfo :--> PTxOut)
getInputUTxO = phoistAcyclic $ plam $ \ credential txIns ->
    precList    (\self txIn rest -> let getCredential txInInfo = pfromData $ pfield @"credential" #$ pfield @"address" #$ pfield @"resolved" # txInInfo
                                    in  pif (getCredential txIn #== credential)
                                            (precList   (\self' txIn' rest' -> pif  (getCredential txIn' #== credential)
                                                                                    (ptraceError "Found more than 1 UTxO! @getInputUTxO")
                                                                                    (self' # rest') )
                                                        (\_ -> pfield @"resolved" # txIn)
                                                        # rest )
                                            (self # rest) )
                (\_ -> ptraceError "Can't find UTxO! @getInputUTxO")
                # txIns

---------------------------------------------------

getInputUTxORef :: Term s (PCredential :--> PBuiltinList PTxInInfo :--> PTxOutRef)
getInputUTxORef = phoistAcyclic $ plam $ \ credential txIns ->
    precList    (\self txIn rest -> let getCredential txInInfo = pfromData $ pfield @"credential" #$ pfield @"address" #$ pfield @"resolved" # txInInfo
                                    in  pif (getCredential txIn #== credential)
                                            (precList   (\self' txIn' rest' -> pif  (getCredential txIn' #== credential)
                                                                                    (ptraceError "Found more than 1 UTxO! @getInputUTxORef")
                                                                                    (self' # rest') )
                                                        (\_ -> pfield @"outRef" # txIn)
                                                        # rest )
                                            (self # rest) )
                (\_ -> ptraceError "Can't find UTxO! @getInputUTxORef")
                # txIns

---------------------------------------------------

getOutputUTxO :: Term s (PAddress :--> PBuiltinList PTxOut :--> PTxOut)
getOutputUTxO = phoistAcyclic $ plam $ \ addr txOuts ->
    precList    (\self txOut rest -> pif    (pfield @"address" # txOut #== addr)
                                            (precList   (\self' txOut' rest' -> pif (pfield @"address" # txOut' #== addr)
                                                                                    (ptraceError "Found more than 1 UTxO! @getOutputUTxO")
                                                                                    (self' # rest') )
                                                        (\_ -> txOut)
                                                        # rest )
                                            (self # rest) )
                (\_ -> ptraceError "Can't find UTxO @getOutputUTxO")
                # txOuts

---------------------------------------------------- | Serializations | ----------------------------------------------------

pwrapValidator :: Term s ((a :--> b :--> PScriptContext :--> PBool) :--> PValidator)
pwrapValidator = phoistAcyclic $
    plam $
        \validator dat red ctx ->
            pif
                (validator # (punsafeCoerce dat) # (punsafeCoerce red) # ctx)
                (popaque $ pconstant ())
                (ptraceError "Validator script evaluation: False")

pwrapPolicy :: Term s ((a :--> PScriptContext :--> PBool) :--> PMintingPolicy)
pwrapPolicy = phoistAcyclic $
    plam $
        \mintingPolicy red ctx ->
            pif
                (mintingPolicy # (punsafeCoerce red) # ctx)
                (popaque $ pconstant ())
                (ptraceError "Minting script evaluation: False")

closedTermToScript :: (forall (s :: S). Term s a) -> Script
closedTermToScript a = case compile (Config DoTracing) a of
    Left (show -> e) -> error e
    Right script -> script

hashScript :: Script -> Term s PScriptHash
hashScript = pconstant . scriptHash

mkScriptCredential :: Term s PScriptHash -> Term s PCredential
mkScriptCredential scriptHash = pcon . PScriptCredential $ pdcons # (pdata scriptHash) # pdnil

mkAddress :: Term s PCredential -> Term s (PMaybeData PStakingCredential) -> Term s PAddress
mkAddress scriptCredential stakingCredential = pcon . PAddress   $ pdcons # pdata scriptCredential
                                                                #$ pdcons # pdata stakingCredential
                                                                #  pdnil

scriptToCBOR :: Script -> Text
scriptToCBOR = Text.decodeUtf8 . B16.encode . SBS.fromShort . serialiseScript

------------------------------------------------- | Write Script to File | -------------------------------------------------

writeScriptToFile :: Script -> String -> IO ()
writeScriptToFile script name = do
    LBS.writeFile filepath content
    putStrLn $ "Wrote \ESC[94m" <> name <> "\ESC[0m to " <> filepath
    putStrLn $ "ScriptHash \ESC[93m" <> show (plift $ hashScript script) <> "\ESC[0m"
    where
        filepath = "Hexxagon-Game-Atlas/Scripts/" <> name <> ".json"
        content  = encodePretty $ object pairs
        pairs    =  [ "type"        .= ("PlutusScriptV2" :: String)
                    , "description" .= name
                    , "cborHex"     .= scriptToCBOR script
                    ]

----------------------------------------------------------------------------------------------------------------------------