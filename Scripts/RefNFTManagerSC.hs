module  RefNFTManagerSC ( typedValidator
                        , validator
                        , script
                        , scriptHash
                        , scriptCredential
                        , address
                        ) where

import Plutarch (Term, Script, (#), pcon, phoistAcyclic, plam, type (:-->))
import Plutarch.Api.V2 (PValidator, PScriptHash, PAddress, PMaybeData (..), PStakingCredential, PScriptContext)
import Plutarch.Api.V1.Address (PCredential)
import PUtilities
import Plutarch.DataRepr (pdnil)
import Plutarch.Prelude (pconstant, PData, PBool)

typedValidator :: Term s (PData :--> PData :--> PScriptContext :--> PBool)
typedValidator = phoistAcyclic $ plam $ \ _dat _red _scriptContext -> pconstant True

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