module  PUtilities  ( pwrapValidator
                    , pwrapPolicy
                    , validatorToScript
                    , hashScript
                    , mkScriptCredential
                    , mkAddress
                    , pAnd'
                    , toPOutputDatum
                    , elemNFT
                    , getOwnAddress
                    , getInputValue
                    , getInputUTxO
                    , getOutputUTxO
                    ) where

import  Plutarch.Prelude
import  Plutarch
import  Plutarch.Monadic            qualified as P
import  Plutarch.Bool               ( pand' )
import  Plutarch.Api.V1             ( PCredential (..) )
import  Plutarch.Api.V1.AssocMap    ( plookup )
import  Plutarch.Api.V2
import  Plutarch.Unsafe             ( punsafeCoerce )

--------------------------------------------------- | Helper Functions | ---------------------------------------------------

pAnd' :: Foldable t => t (Term s PBool) -> Term s PBool
pAnd' = foldr ((#) . (pand' #)) (pconstant True)

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

getOutputUTxO :: Term s (PAddress :--> PBuiltinList PTxOut :--> PTxOut)
getOutputUTxO = phoistAcyclic $ plam $ \ addr txOuts ->
    precList    (\self txOut rest ->    let txOutAddr = pfield @"address" # txOut
                                        in  pif (txOutAddr #== addr)
                                                (txOut)
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

validatorToScript :: (forall (s :: S). Term s PValidator) -> Script
validatorToScript validator = case compile (Config DoTracing) validator of
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
