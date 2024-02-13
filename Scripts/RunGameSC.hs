module  RunGameSC   ( typedValidator
                    , validator
                    , script
                    , scriptHash
                    , address
                    ) where

import Plutarch.Prelude
import Plutarch.Api.V2.Contexts
import PDataTypes (PGameInfo(..), PRunGame (..))
import Plutarch.Api.V2 ( PStakingCredential, PAddress(..), PMaybeData(PDNothing), PValidator, PScriptHash )
import Plutarch.Api.V1.Address (PCredential)
import PUtilities (pwrapValidator, validatorToScript, hashScript, mkScriptCredential, mkAddress)
import Plutarch.Script (Script)

----------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------ | Validator | -------------------------------------------------------

typedValidator :: Term s (PGameInfo :--> PRunGame :--> PScriptContext :--> PBool)
typedValidator = phoistAcyclic $ plam $ \ _gameInfo _runGame _scriptContext -> perror

--------------------------------------------------- | Helper Functions | ---------------------------------------------------

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

----------------------------------------------------- | Benchmarking | -----------------------------------------------------

------------------------------------------------------- End Of Code --------------------------------------------------------