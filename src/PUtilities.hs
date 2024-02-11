module  PUtilities  ( pwrapValidator
                    , pwrapPolicy
                    , validatorToScript
                    ) where

import  Plutarch            ( S, Term, type (:-->), Config(Config), Script, (#), compile, phoistAcyclic, TracingMode(DoTracing), popaque, plam )
import  Plutarch.Api.V2     ( PMintingPolicy, PValidator, PScriptContext )
import  Plutarch.Prelude    ( PBool, pif, pconstant, ptraceError )
import  Plutarch.Unsafe     ( punsafeCoerce )

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
