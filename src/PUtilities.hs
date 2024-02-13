module  PUtilities  ( pwrapValidator
                    , pwrapPolicy
                    , validatorToScript
                    , hashScript
                    , mkScriptCredential
                    , mkAddress
                    ) where

import  Plutarch            ( S, Term, type (:-->), Config(Config), Script, (#), compile, phoistAcyclic, TracingMode(DoTracing), popaque, plam )
import  Plutarch.Api.V2     ( PMintingPolicy, PValidator, PScriptContext, PScriptHash (..), PAddress (..), PMaybeData, PStakingCredential, scriptHash )
import  Plutarch.Prelude    ( PBool, pif, pconstant, ptraceError, pcon, pdcons, pdata, pdnil, (#$) )
import  Plutarch.Unsafe     ( punsafeCoerce )
import  Plutarch.Api.V1     ( PCredential (..) )

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
