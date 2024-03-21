module  UnitTesting.TestRefNFTManagerSC ( testAllConditions
                                        , allTestCases
                                        ) where

import Plutarch.Prelude
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2
import Plutarch.Context
import Test.Tasty
import UnitTesting.Tools
import UnitTesting.TConstants
import DataTypes
import Data.List ((\\))
import qualified PlutusTx.AssocMap as AssocMap
import qualified RunGameSC
import qualified RefNFTManagerSC

------------------------------------------------------ | Unit Tests | ------------------------------------------------------

testAllConditions :: TestTree
testAllConditions = testGroup "Unit Testing : RefNFTManagerSC Validator"
    [ testGroup "RefNFTManagerSC : ()" (testElement RefNFTManagerSC.validator . getTestCase <$> conditions)
    ]

------------------------------------------------------ Conditions

allConditions :: [TestElement]
allConditions = concat [conditions]

conditions :: [TestElement]
conditions =    [ Condition 1  " 1 - Own Input (RefNFTManagerSC) is valid: Value Contains the right RefNFT."
                , Condition 2  " 2a - Output to RefNFTManagerSC has: Value preserved (2ADA + RefNFT)."
                , Condition 3  " 2b - Output to RefNFTManagerSC has: Datum is the newly calculated Metadata."
                ]

------------------------------------------------------ Test Cases

allTestCases :: [(TestElement,[Arguments])]
allTestCases = getTestCase <$> allConditions

getTestCase :: TestElement -> (TestElement,[Arguments])
getTestCase testElem = (testElem,) arguments
    where
        arguments = recFromJust $ getArguments testElem.number <$> [1..]
        recFromJust ((Just x):xs) = x : recFromJust xs
        recFromJust _ = []

------------------------------------------------------ Arguments: Datum, Redeemer, Script Context

getArguments :: Int -> Int -> Maybe Arguments
getArguments 01 1 = Just $ Right [toData initialMetadata , toData () , toData $ scriptContext 01]
getArguments 01 2 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 02]
getArguments 01 3 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 03]
getArguments 01 4 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 04]
getArguments 01 5 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 05]
getArguments 01 6 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 06]
getArguments 01 7 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 07]
getArguments 01 8 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 08]
getArguments 01 9 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 09]
getArguments 02 1 = Just $ Right [toData initialMetadata , toData () , toData $ scriptContext 10]
getArguments 02 2 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 11]
getArguments 02 3 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 12]
getArguments 02 4 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 13]
getArguments 02 5 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 14]
getArguments 02 6 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 15]
getArguments 02 7 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 16]
getArguments 02 8 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext 17]
getArguments 03 1 = Just $ Right [toData initialMetadata , toData () , toData $ scriptContext 18]
getArguments 03 n | n <= 17 = Just $ Left  [toData initialMetadata , toData () , toData $ scriptContext (n+17)]
getArguments 03 18 = Just $ Left [toData initialMetadata , toData () , toData $ scriptContext 35]
getArguments 03 19 = Just $ Left [toData initialMetadata , toData () , toData $ scriptContext 36]

getArguments _ _ = Nothing

--------------------------------------------------- | Sample Variables | ---------------------------------------------------

sHashRunGameSC, sHashRefNFTManagerSC :: ScriptHash
sHashRunGameSC = plift RunGameSC.scriptHash
sHashRefNFTManagerSC = plift RefNFTManagerSC.scriptHash

scriptContextConstantPart :: [SpendingBuilder]
scriptContextConstantPart = [ inputScript sHashRunGameSC redeemer   200     []                  10
                            , inputPubKey   bobPaymentAddress       2       [identifierNFT_B]   11
                            , outputValue   bobPaymentAddress       200     [identifierNFT_B]
                            , signedWith $ samplePubKeyHash "Bob"
                            , txId $ makeTxId "Unique Random Text!"
                            , fee (singleton adaSymbol adaToken 2)
                            , withSpendingOutRefIdx 0
                            ] where redeemer = GameOver playerB

-- Datum : Metadata

initialMetadata, newMetadata :: Metadata
initialMetadata = Metadata hexxagonMetadata 2 (List [])
    where
        hexxagonMetadata = AssocMap.fromList $  [ ("name"           , B "Hexxagon Game"                                          )
                                                , ("image"          , B "ipfs://QmcNdgGVQ5Yw9ckWH2PohYKyvmud2MaksyzHz1SBAoM89h"  )
                                                , ("description"    , B "Hexxagon game on the Cardano Blockchain"                )
                                                , ("authors"        , B "Dr. Mazen Khaddaj & Andrew Garrett Wright"              )
                                                , ("score"          , I 0                                                        )
                                                ]

newMetadata = Metadata (AssocMap.insert "score" (I 1) $ initialMetadata.getMetadata) 2 (List [])

compromisedMetadata :: Int -> Metadata
compromisedMetadata 01 = Metadata (AssocMap.insert "score" (I 0)     $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 02 = Metadata (AssocMap.insert "score" (I 2)     $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 03 = Metadata (AssocMap.insert "score" (B "0")   $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 04 = Metadata (AssocMap.insert "score" (B "1")   $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 05 = Metadata (AssocMap.insert "score" (B "2")   $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 06 = Metadata (AssocMap.insert "score" (B "")    $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 07 = Metadata (AssocMap.insert "score" (List []) $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 08 = Metadata (AssocMap.delete "score" $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 09 = Metadata (AssocMap.delete "name"  $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 10 = Metadata (AssocMap.delete "image" $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 11 = Metadata (AssocMap.insert "extra" (B "") $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 12 = Metadata (AssocMap.insert "name" (B "Hexxagon Gam") $ initialMetadata.getMetadata) 2 (List [])
compromisedMetadata 13 = Metadata (AssocMap.fromList []) 2 (List [])
compromisedMetadata 14 = Metadata initialMetadata.getMetadata 1 (List [])
compromisedMetadata 15 = Metadata initialMetadata.getMetadata 2 (Map [])
compromisedMetadata 16 = Metadata initialMetadata.getMetadata 2 (I 0)

compromisedMetadata _ = error "Undefined compromisedMetadata!"

---------------------------------------------------- | Script Context | ----------------------------------------------------

scriptContext :: Int -> ScriptContext

-- Value of Input-UTxO @RefNFTManagerSC Contains the right RefNFT (Success case)
scriptContext 1 = mkScriptContext_RefNFT [refNFT_B]

-- Value of Input-UTxO @RefNFTManagerSC Contains the Wrong RefNFT (Fail cases)
scriptContext 2 = mkScriptContext_RefNFT [refNFT_A]
scriptContext 3 = mkScriptContext_RefNFT [identifierNFT_B]
scriptContext 4 = mkScriptContext_RefNFT [identifierNFT_A]
scriptContext 5 = mkScriptContext_RefNFT [identifierNFT_C]
scriptContext 6 = mkScriptContext_RefNFT []
scriptContext 7 = mkScriptContext_RefNFT [refNFT_B,refNFT_B]
scriptContext 8 = mkScriptContext_RefNFT [refNFT_B,identifierNFT_B]
scriptContext 9 = mkScriptContext_RefNFT [identifierNFT_B,identifierNFT_B]

-- Output to RefNFTManagerSC has: Value preserved (2ADA + RefNFT) (Success case)
scriptContext 10 = mkScriptContext_OutputValue 2_000_000 [refNFT_B]

-- Output to RefNFTManagerSC has: different Value (Fail cases)
scriptContext 11 = mkScriptContext_OutputValue 1         [refNFT_B]
scriptContext 12 = mkScriptContext_OutputValue 1_999_999 [refNFT_B]
scriptContext 13 = mkScriptContext_OutputValue 2_000_001 [refNFT_B]
scriptContext 14 = mkScriptContext_OutputValue 2_000_000 []
scriptContext 15 = mkScriptContext_OutputValue 2_000_000 [refNFT_A]
scriptContext 16 = mkScriptContext_OutputValue 2_000_000 [identifierNFT_B]
scriptContext 17 = mkScriptContext_OutputValue 2_000_000 [refNFT_B,refNFT_A]

-- Output to RefNFTManagerSC has: Datum is the correctly calculated newMetadata (Success case)
scriptContext 18 = mkScriptContext_Metadata newMetadata

-- Output to RefNFTManagerSC has: compromised Datum (Fail cases)
scriptContext n | n <= 18 + 16 = mkScriptContext_Metadata $ compromisedMetadata (n - 18)
scriptContext 35 = mkScriptContext_Metadata ()
scriptContext 36 = mkScriptContext_Metadata newMetadata.getMetadata

scriptContext _ = error "Undefined Script Context!"

----------------------------------------------- | Script Context Functions | -----------------------------------------------

-- Player B (Bob) ending the game by consuming the UTxO at RunGameSC & updating Metadata
-- Success or failure (depending on refNFT at value of consumed UTxO from RefNFTManagerSC)
mkScriptContext_RefNFT :: [AssetClass] -> ScriptContext
mkScriptContext_RefNFT refNFT = buildSpending checkPhase1 $ mconcat $
            [ inputScript   sHashRefNFTManagerSC ()     2_000_000   refNFT            0
            , outputScript  refNFTManagerSCAddress      2_000_000   refNFT newMetadata
            ] <> scriptContextConstantPart
            where
                refNFTManagerSCAddress = plift RefNFTManagerSC.address

-- Success or failure (depending on ADA & non-ADA assets at value of Output UTxO to RefNFTManagerSC)
mkScriptContext_OutputValue :: Integer -> [AssetClass] -> ScriptContext
mkScriptContext_OutputValue adaOutput nonADAOutput = buildSpending checkPhase1 $ mconcat $
            [ inputScript   sHashRefNFTManagerSC ()     2_000_000   [refNFT_B]      0
            , inputPubKey   bobPaymentAddress           (adaInput  +1)  nonADAInput     1
            , outputScript  refNFTManagerSCAddress      adaOutput       nonADAOutput newMetadata
            , outputValue   bobPaymentAddress           (adaChange +1)  nonADAChange
            ] <> scriptContextConstantPart
            where
                refNFTManagerSCAddress = plift RefNFTManagerSC.address
                adaChange = if adaOutput < 2_000_000 then 2_000_000 - adaOutput else 0
                adaInput  = if adaOutput > 2_000_000 then adaOutput - 2_000_000 else 0
                nonADAInput  = nonADAOutput \\ [refNFT_B]
                nonADAChange = [refNFT_B] \\ nonADAOutput

-- Success or failure (depending on Datum of Output UTxO to RefNFTManagerSC)
mkScriptContext_Metadata :: ToData datum => datum -> ScriptContext
mkScriptContext_Metadata metadata = buildSpending checkPhase1 $ mconcat $
            [ inputScript   sHashRefNFTManagerSC ()     2_000_000   [refNFT_B]      0
            , outputScript  refNFTManagerSCAddress      2_000_000   [refNFT_B]  metadata
            ] <> scriptContextConstantPart
            where
                refNFTManagerSCAddress = plift RefNFTManagerSC.address

----------------------------------------------------------------------------------------------------------------------------