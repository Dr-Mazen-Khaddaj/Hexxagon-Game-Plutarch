module  UnitTesting.TestInitialiseGameSC    ( testAllConditions
                                            , allTestCases
                                            ) where

import  Test.Tasty                  (TestTree, testGroup)
import  Plutarch.Prelude            (plift)
import  PlutusTx                    (toData)
import  DataTypes                   (Initialization (Add, Withdraw), GameInfo (..), GameSettings (Settings), GameState (Game))
import  PlutusLedgerApi.V2          (Interval (..), ScriptContext, singleton, adaSymbol, adaToken, LowerBound (..), Extended (..), UpperBound (..), ScriptHash)
import  UnitTesting.Tools
import  Plutarch.Context            (signedWith, txId, fee, withSpendingOutRefIdx, timeRange, buildSpending, checkPhase1)
import  UnitTesting.TConstants
import  Constants                   (classicBoard_S9DC3, classicBoard_S5DC3)
import  InitialiseGameSC            qualified
import  RunGameSC                   qualified

------------------------------------------------------ | Unit Tests | ------------------------------------------------------

testAllConditions :: TestTree
testAllConditions = testGroup "Unit Testing : InitialiseGameSC Validator"
    [ testGroup "Initialization : Start Game"  (testElement InitialiseGameSC.validator . getTestCase <$> conditions_StartGame)
    , testGroup "Initialization : Cancel Game" (testElement InitialiseGameSC.validator . getTestCase <$> conditions_CancelGame)
    ]

------------------------------------------------------ Conditions

allConditions :: [TestElement]
allConditions = concat  [ conditions_StartGame
                        , conditions_CancelGame
                        ]

conditions_StartGame, conditions_CancelGame :: [TestElement]
conditions_StartGame =  [ Condition 1  " 1 - Only 1 UTxO consumed from InitialiseGameSC."
                        , Condition 2  " 2 - New UTxO is created at RunGameSC"
                        , Condition 3  " 3 - Value doubled"
                        , Condition 4  " 4a - Correct Datum : right players"
                        , Condition 5  " 4b - Correct Datum : same turn duration"
                        , Condition 6  " 4c - Correct Datum : right player's turn"
                        , Condition 7  " 4d - Correct Datum : correct Deadline"
                        , Condition 8  " 4e - Correct Datum : same board"
                        , Condition 9  " 5a - Legal Redeemer : Color Red"
                        , Condition 10 " 5b - Legal Redeemer : Registering a unique NFT."
                        ]

conditions_CancelGame = [ Condition 11 "Cancel Game : The registered NFT is found in the input UTxOs."
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
getArguments 01 1 = Just $ Right [toData normalSettings , toData (Add playerB) , toData $ scriptContext 01]
getArguments 01 2 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 02]
getArguments 02 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 03]
getArguments 02 2 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 04]
getArguments 03 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 05]
getArguments 03 2 = Just $ Right [toData normalSettings , toData (Add playerB) , toData $ scriptContext 06]
getArguments 03 3 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 07]
getArguments 03 4 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 08]
getArguments 03 5 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 09]
getArguments 04 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 10]
getArguments 04 2 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 11]
getArguments 04 3 = Just $ Left  [toData normalSettings , toData (Add playerA) , toData $ scriptContext 12]
getArguments 04 4 = Just $ Left  [toData normalSettings , toData (Add playerX) , toData $ scriptContext 13]
getArguments 04 5 = Just $ Left  [toData normalSettings , toData (Add playerY) , toData $ scriptContext 14]
getArguments 05 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 15]
getArguments 05 2 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 16]
getArguments 06 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 17]
getArguments 07 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 18]
getArguments 08 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 19]
getArguments 09 1 = Just $ Left  [toData normalSettings , toData (Add playerA) , toData $ scriptContext 12]
getArguments 09 2 = Just $ Left  [toData normalSettings , toData (Add playerY) , toData $ scriptContext 14]
getArguments 10 1 = Just $ Left  [toData normalSettings , toData (Add playerX) , toData $ scriptContext 13]
getArguments 11 1 = Just $ Right [toData normalSettings , toData Withdraw      , toData $ scriptContext 20]
getArguments 11 2 = Just $ Left  [toData normalSettings , toData Withdraw      , toData $ scriptContext 21]
getArguments 11 3 = Just $ Left  [toData normalSettings , toData Withdraw      , toData $ scriptContext 22]
getArguments _ _ = Nothing

--------------------------------------------------- | Sample Variables | ---------------------------------------------------

scriptH :: ScriptHash
scriptH = plift InitialiseGameSC.scriptHash

-- Datum : Player A created a game with those settings
normalSettings :: GameSettings
normalSettings = Settings playerA turnDuration7h classicBoard_S9DC3

---------------------------------------------------- | Script Context | ----------------------------------------------------

scriptContext :: Int -> ScriptContext

---------------------------------------------------- Start Game

-- Player B (Bob) starting the game by consuming only one UTxO (Normal case)
scriptContext 1 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputPubKey   bobPaymentAddress           2       []      1
            , inputPubKey   bobPaymentAddress           100     []      2
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Player B trying to consuming 2 UTxOs (Double Satisfaction Attack)
scriptContext 2 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputScript   scriptH (Add playerB)       100     []      1           -- Player B (Bob) trying to steal 100 Lovelace from this UTxO
            , inputPubKey   bobPaymentAddress           2       []      2
            , inputPubKey   bobPaymentAddress           100     []      3
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , outputLovelace bobPaymentAddress          100                         -- Taking the 100 Lovelace to his wallet
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Player B trying to consume the UTxO without sending it to RunGameSC (A naive attack)
scriptContext 3 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputPubKey   bobPaymentAddress           2       []      1
            , outputLovelace bobPaymentAddress          100                         -- Taking the 100 Lovelace to his wallet
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Player B trying to send the UTxO to a compromised RunGameSC address that has his own staking credential (Staking-Key-Control Attack)
scriptContext 4 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputPubKey   bobPaymentAddress           102     []      1
            , outputScript  compromisedRunGameSCAddress 200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Output UTxO has less Lovelace (Illegal Value)
scriptContext 5 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputPubKey   bobPaymentAddress           92      []      1
            , outputScript  (plift RunGameSC.address)   190     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Output Value contain tokens (legal Value)
scriptContext 6 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     [clayNFT]       0
            , inputPubKey   bobPaymentAddress           102     [clayNFT]       1
            , outputScript  (plift RunGameSC.address)   200     [clayNFT,clayNFT]  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Output Value contain missing token (Illegal Value)
scriptContext 7 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     [clayNFT]       0
            , inputPubKey   bobPaymentAddress           102     []              1
            , outputScript  (plift RunGameSC.address)   200     [clayNFT]  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Output Value contain different token (Illegal Value)
scriptContext 8 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     [clayNFT]       0
            , inputPubKey   bobPaymentAddress           102     [ticketNFT]     1
            , outputScript  (plift RunGameSC.address)   200     [clayNFT,ticketNFT]  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Output Value contain extra token (Illegal Value)
scriptContext 9 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []          0
            , inputPubKey   bobPaymentAddress           102     [clayNFT]   1
            , outputScript  (plift RunGameSC.address)   200     [clayNFT]  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Output datum has missing player A (Datum : wrong Players)
scriptContext 10 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputPubKey   bobPaymentAddress           102     []      1
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerB] turnDuration7h gameStateS0 -- missing player A
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Output datum has players in different order (Datum : wrong Players)
scriptContext 11 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputPubKey   bobPaymentAddress           102     []      1
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerB,playerA] turnDuration7h gameStateS0 -- different order
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Output datum has same player twice (Datum : wrong Players)
scriptContext 12 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerA)       100     []      0           -- Adding PlayerA
            , inputPubKey   bobPaymentAddress           102     []      1
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerA (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerA] turnDuration7h gameStateS0 -- duplicate player A
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Second player holds same NFT (Datum : wrong Players)
scriptContext 13 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerX)       100     []      0           -- PlayerC has same NFT as player A
            , inputPubKey   bobPaymentAddress           102     []      1
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerX (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerX] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Second player has same color as player A (Datum : wrong Players)
scriptContext 14 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerY)       100     []      0           -- playerD has color Blue, and different NFT
            , inputPubKey   bobPaymentAddress           102     []      1
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerY (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerY] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Output datum has different turnDuration (Datum : wrong turnDuration)
scriptContext 15 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputPubKey   bobPaymentAddress           102     []      1
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerA (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration1h gameStateS0 -- From 7h to 1h
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Output datum has different turnDuration & calculated deadline accordingly (Datum : wrong turnDuration)
scriptContext 16 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputPubKey   bobPaymentAddress           102     []      1
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerA (currentTime2024Jan1 + turnDuration1h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration1h gameStateS0 -- From 7h to 1h
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- PlayerA's turn (Datum : wrong player'sTurn)
scriptContext 17 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputPubKey   bobPaymentAddress           2       []      1
            , inputPubKey   bobPaymentAddress           100     []      2
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerA (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- compromised deadline (Datum : wrong deadline)
scriptContext 18 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputPubKey   bobPaymentAddress           2       []      1
            , inputPubKey   bobPaymentAddress           100     []      2
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration1h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Different BoardS0 (Datum : wrong Board)
scriptContext 19 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerB)       100     []      0
            , inputPubKey   bobPaymentAddress           2       []      1
            , inputPubKey   bobPaymentAddress           100     []      2
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerB (currentTime2024Jan1 + turnDuration7h) classicBoard_S5DC3
                normalGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

---------------------------------------------------- Cancel Game

-- Player A (Alice) has the registered NFT (identifierNFT_A) in the input UTxOs. (Normal case)
scriptContext 20 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH Withdraw        100     []      0
            , inputPubKey   alicePaymentAddress     2       [nft]   1
            , outputValue   alicePaymentAddress     100     [nft]
            , signedWith $ samplePubKeyHash "Alice"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            ]
            where
                nft = identifierNFT_A

-- Player A missing identifier NFT (missing identifierNFT)
scriptContext 21 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH Withdraw        100     []      0
            , inputPubKey   alicePaymentAddress     2       []      1
            , outputValue   alicePaymentAddress     100     []
            , signedWith $ samplePubKeyHash "Alice"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            ]

-- player A showing an unregistered NFT (wrong identifierNFT)
scriptContext 22 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH Withdraw        100     []      0
            , inputPubKey   alicePaymentAddress     2       [nft]   1
            , outputValue   alicePaymentAddress     100     [nft]
            , signedWith $ samplePubKeyHash "Alice"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            ]
            where
                nft = identifierNFT_C

scriptContext _ = error "Undefined Script Context!"