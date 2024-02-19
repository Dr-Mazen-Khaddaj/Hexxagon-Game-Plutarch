module  UnitTesting.TestInitialiseGameSC (testAllConditions, scriptContext) where

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
testAllConditions = testGroup "Unit Testing : InitialiseGameSC.validator" $
    testElement InitialiseGameSC.validator getArguments <$>
        [ Condition 1 "Only 1 UTxO consumed from InitialiseGameSC."
        , Condition 2 "New UTxO is created at RunGameSC"
        , Condition 3 "Value doubled"
        , Condition 4 "Correct Datum : right players"
        , Condition 5 "Correct Datum : same turn duration"
        , Condition 6 "Correct Datum : right player's turn"
        , Condition 7 "Correct Datum : correct Deadline"
        , Condition 8 "Correct Datum : same board"
        , Condition 9 "Cancel Game : The registered NFT is found in the input UTxOs."
        ]

-- Arguments: Datum, Redeemer, Script Context

getArguments :: Int -> Int -> Maybe Arguments
getArguments 1 1 = Just $ Right [toData normalSettings , toData (Add playerB) , toData $ scriptContext 01] -- 1 Succeed
getArguments 1 2 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 02] -- 2 Fail
getArguments 2 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 03] -- 1 Fail
getArguments 2 2 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 04] -- 1 Fail
getArguments 3 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 05] -- 1 Fail
getArguments 3 2 = Just $ Right [toData normalSettings , toData (Add playerB) , toData $ scriptContext 06] -- 1 Succeed
getArguments 3 3 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 07] -- 1 Fail
getArguments 3 4 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 08] -- 1 Fail
getArguments 3 5 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 09] -- 1 Fail
getArguments 4 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 10] -- 1 Fail
getArguments 4 2 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 11] -- 1 Fail
getArguments 4 3 = Just $ Left  [toData normalSettings , toData (Add playerA) , toData $ scriptContext 12] -- 1 Fail
getArguments 4 4 = Just $ Left  [toData normalSettings , toData (Add playerC) , toData $ scriptContext 13] -- 1 Fail
getArguments 4 5 = Just $ Left  [toData normalSettings , toData (Add playerD) , toData $ scriptContext 14] -- 1 Fail
getArguments 5 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 15] -- 1 Fail
getArguments 5 2 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 16] -- 1 Fail
getArguments 6 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 17] -- 1 Fail
getArguments 7 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 18] -- 1 Fail
getArguments 8 1 = Just $ Left  [toData normalSettings , toData (Add playerB) , toData $ scriptContext 19] -- 1 Fail
getArguments 9 1 = Just $ Right [toData normalSettings , toData Withdraw      , toData $ scriptContext 20] -- 1 Succeed
getArguments 9 2 = Just $ Left  [toData normalSettings , toData Withdraw      , toData $ scriptContext 21] -- 1 Fail
getArguments 9 3 = Just $ Left  [toData normalSettings , toData Withdraw      , toData $ scriptContext 22] -- 1 Fail
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

-- Player B trying to send the UTxO to a compromised RunGameSC address that his his own staking credential (Staking-Key-Control Attack)
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
                normalGameInfo = GameInfo [playerB,playerA] turnDuration7h gameStateS0 -- missing player A
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
            [ inputScript   scriptH (Add playerC)       100     []      0           -- PlayerC has same NFT as player A
            , inputPubKey   bobPaymentAddress           102     []      1
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerC (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerC] turnDuration7h gameStateS0
                validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

-- Second player has same color as player A (Datum : wrong Players)
scriptContext 14 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (Add playerD)       100     []      0           -- playerD has color Blue, and different NFT
            , inputPubKey   bobPaymentAddress           102     []      1
            , outputScript  (plift RunGameSC.address)   200     []  normalGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange validRange
            ]
            where
                gameStateS0 = Game playerD (currentTime2024Jan1 + turnDuration7h) classicBoard_S9DC3
                normalGameInfo = GameInfo [playerA,playerD] turnDuration7h gameStateS0
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