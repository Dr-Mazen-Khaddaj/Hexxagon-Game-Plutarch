module Main (main) where

import  Test.Tasty (TestTree, testGroup, defaultMain)
import  UnitTesting.TestInitialiseGameSC    qualified
import  UnitTesting.TestRunGameSC           qualified

main :: IO ()
main = defaultMain $ testGroup "All Tests" [unitTests, propertyTests]

-- Unit Testing
unitTests :: TestTree
unitTests = testGroup "All Unit Tests"
    [ UnitTesting.TestInitialiseGameSC.testAllConditions
    , UnitTesting.TestRunGameSC.testAllConditions
    ]

-- Property Testing
propertyTests :: TestTree
propertyTests = testGroup "All Property Tests" []