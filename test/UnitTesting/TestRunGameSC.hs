module  UnitTesting.TestRunGameSC   ( testAllConditions
                                    , allTestCases
                                    ) where

import Test.Tasty
import UnitTesting.Tools
import PlutusLedgerApi.V2
import DataTypes
import Plutarch.Prelude
import Plutarch.Context
import UnitTesting.TConstants
import Constants
import qualified RunGameSC
import UtilityFxs (makeMove, unsafeMakeMove)

------------------------------------------------------ | Unit Tests | ------------------------------------------------------

testAllConditions :: TestTree
testAllConditions = testGroup "Unit Testing : RunGameSC Validator"
    [ testGroup "RunGame : PlayTurn" (testElement RunGameSC.validator . getTestCase <$> conditions_PlayTurn)
    , testGroup "RunGame : GameOver" (testElement RunGameSC.validator . getTestCase <$> conditions_GameOver)
    , testGroup "RunGame : TimeOut"  (testElement RunGameSC.validator . getTestCase <$> conditions_TimeOut)
    ]

------------------------------------------------------ Conditions

allConditions :: [TestElement]
allConditions = concat [conditions_PlayTurn, conditions_GameOver, conditions_TimeOut]

conditions_PlayTurn, conditions_GameOver, conditions_TimeOut :: [TestElement]

conditions_PlayTurn =   [ Condition 1  " 1 - Only 1 UTxO consumed from RunGameSC."
                        , Condition 2  " 2 - Continuing Output: Own UTxO is sent to own smart contract."
                        , Condition 3  " 3 - Value Preserved"
                        , Condition 4  " 4a - Correct Datum : same players"
                        , Condition 5  " 4b - Correct Datum : same turn duration"
                        , Condition 6  " 4c - Correct Datum : correct player's turn"
                        , Condition 7  " 4d - Correct Datum : correct new Deadline"
                        , Condition 8  " 4e - Correct Datum : correct new board"
                        , Condition 9  " 5 - Before current deadline"
                        , Condition 10 " 6 - Player's registered NFT is found in the input UTxOs."
                        ]

conditions_GameOver = []
conditions_TimeOut  = []

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
getArguments 01 1 = Just $ Right [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 01]
getArguments 01 2 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 02]
getArguments 02 1 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 03]
getArguments 02 2 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 04]
getArguments 03 1 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 05]
getArguments 04 1 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 06]
getArguments 04 2 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 07]
getArguments 04 3 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 08]
getArguments 05 1 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 09]
getArguments 05 2 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 10]
getArguments 06 1 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 11]
getArguments 07 1 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 12]
getArguments 08 1 = Just $ Right [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 13]
getArguments 08 2 = Just $ Right [toData normalGameInfo , toData (PlayTurn legalMove2)   , toData $ scriptContext 14]
getArguments 08 3 = Just $ Left  [toData normalGameInfo , toData (PlayTurn illegalMove1) , toData $ scriptContext 15]
getArguments 08 4 = Just $ Left  [toData normalGameInfo , toData (PlayTurn illegalMove2) , toData $ scriptContext 16]
getArguments 08 5 = Just $ Left  [toData normalGameInfo , toData (PlayTurn illegalMove3) , toData $ scriptContext 17]
getArguments 08 6 = Just $ Left  [toData normalGameInfo , toData (PlayTurn illegalMove4) , toData $ scriptContext 18]
getArguments 09 1 = Just $ Right [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 19]
getArguments 09 2 = Just $ Right [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 20]
getArguments 09 3 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 21]
getArguments 10 1 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 22]
getArguments 10 2 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 23]
getArguments 10 3 = Just $ Left  [toData normalGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 24]
getArguments _ _ = Nothing

--------------------------------------------------- | Sample Variables | ---------------------------------------------------

scriptH :: ScriptHash
scriptH = plift RunGameSC.scriptHash

newBoard :: Board
newBoard = makeMove legalMove1 classicBoard_S9DC3

gameStateS1 :: GameState
gameStateS1 = Game playerA (currentTime2024Jan1 + 2*turnDuration7h) newBoard

newGameInfo :: GameInfo
newGameInfo = GameInfo [playerA,playerB] turnDuration7h gameStateS1

makeGameInfoFromMove :: Move -> GameInfo
makeGameInfoFromMove move = GameInfo [playerA,playerB] turnDuration7h gameState
    where
        gameState = Game playerA (currentTime2024Jan1 + 2*turnDuration7h) board
        board = unsafeMakeMove Red move classicBoard_S9DC3

validRange :: Interval POSIXTime
validRange = Interval (LowerBound (Finite currentTime2024Jan1) True) (UpperBound (PosInf) False)

scriptContextConstantPart :: [SpendingBuilder]
scriptContextConstantPart = [ inputScript   scriptH normalPlayTurn 200 [] 0
                            , signedWith $ samplePubKeyHash "Bob"
                            , txId $ makeTxId "Unique Random Text!"
                            , fee (singleton adaSymbol adaToken 2)
                            , withSpendingOutRefIdx 0
                            , timeRange validRange
                            ]


-- Datum : PlayerB's turn after he initiated the game (1st move)

normalGameInfo :: GameInfo
normalGameInfo = GameInfo [playerA,playerB] turnDuration7h (Game playerB (currentTime2024Jan1+turnDuration7h) classicBoard_S9DC3)

-- Redeemer : Legal move

legalMove1, legalMove2, illegalMove1, illegalMove2, illegalMove3, illegalMove4 :: Move

-- Distance = 1
legalMove1  = Move (Position 1 5) (Position 2 5)

-- Distance = 2
legalMove2  = Move (Position 1 5) (Position 3 5)

-- Distance = 3
illegalMove1  = Move (Position 1 5) (Position 4 6)

-- Initial Position does not exist
illegalMove2  = Move (Position 1 6) (Position 2 6)

-- Initial hexagon is Empty
illegalMove3  = Move (Position 2 5) (Position 3 5)

-- Initial hexagon is Blue
illegalMove4  = Move (Position 1 1) (Position 2 2)

normalPlayTurn :: RunGame
normalPlayTurn = PlayTurn legalMove1

---------------------------------------------------- | Script Context | ----------------------------------------------------

scriptContext :: Int -> ScriptContext

---------------------------------------------------- Play Turn

-- Only 1 UTxO consumed from RunGameSC (Success case)
scriptContext 1 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           2       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  newGameInfo
            , outputNFT     bobPaymentAddress identifierNFT_B
            ] <> scriptContextConstantPart

-- Player B trying to consume 2 UTxOs from RunGameSC (Double Satisfaction Attack)
scriptContext 2 = buildSpending checkPhase1 $ mconcat $
            [ inputScript   scriptH normalPlayTurn      200     []                  1
            , inputPubKey   bobPaymentAddress           2       [identifierNFT_B]   2
            , outputScript  (plift RunGameSC.address)   200     []  newGameInfo
            , outputValue   bobPaymentAddress           200     [identifierNFT_B]
            ] <> scriptContextConstantPart

-- Player B trying to consume the UTxO without sending it to RunGameSC (A naive attack)
scriptContext 3 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           2       [identifierNFT_B]   1
            , outputValue   bobPaymentAddress           200     [identifierNFT_B]
            ] <> scriptContextConstantPart

-- Player B trying to send the UTxO to a compromised RunGameSC address that has his own staking credential (Staking-Key-Control Attack)
scriptContext 4 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  compromisedRunGameSCAddress 200     []  newGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart

-- Output UTxO has less Lovelace (Illegal Value)
scriptContext 5 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   2       []  newGameInfo
            , outputValue   bobPaymentAddress           200     [identifierNFT_B]
            ] <> scriptContextConstantPart

-- Output datum has missing player A (Datum : wrong Players)
scriptContext 6 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart
            where
                compromisedGameInfo = GameInfo [playerB] turnDuration7h gameStateS1

-- Output datum has players in different order (Datum : wrong Players)
scriptContext 7 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart
            where
                compromisedGameInfo = GameInfo [playerB,playerA] turnDuration7h gameStateS1

-- Output datum has different opponent NFT (Datum : wrong Players)
scriptContext 8 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart
            where
                compromisedGameInfo = GameInfo [playerA,playerD] turnDuration7h gameStateS1

-- Output datum has different turnDuration (Datum : wrong turnDuration)
scriptContext 9 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart
            where
                compromisedGameInfo = GameInfo [playerA,playerB] turnDuration1h gameStateS1

-- Output datum has different turnDuration & calculated deadline accordingly (Datum : wrong turnDuration)
scriptContext 10 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart
            where
                compromisedGameStateS1 = Game playerA (currentTime2024Jan1 + 2*turnDuration1h) newBoard
                compromisedGameInfo = GameInfo [playerA,playerB] turnDuration1h compromisedGameStateS1

-- PlayerA's turn (Datum : wrong player'sTurn)
scriptContext 11 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart
            where
                compromisedGameStateS1 = Game playerB (currentTime2024Jan1 + 2*turnDuration7h) newBoard
                compromisedGameInfo = GameInfo [playerA,playerB] turnDuration7h compromisedGameStateS1

-- compromised deadline (Datum : wrong deadline)
scriptContext 12 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart
            where
                compromisedGameStateS1 = Game playerA (currentTime2024Jan1 + turnDuration7h + turnDuration1h) newBoard
                compromisedGameInfo = GameInfo [playerA,playerB] turnDuration7h compromisedGameStateS1

-- Legal Move : Distance = 1 (Datum : correct Board)
scriptContext 13 = mkScriptContext_Move legalMove1

-- Legal Move : Distance = 2 (Datum : correct Board)
scriptContext 14 = mkScriptContext_Move legalMove2

-- Illegal Move : Distance = 3 (Datum : wrong Board)
scriptContext 15 = mkScriptContext_Move illegalMove1

-- Illegal Move : Initial Position does not exist (Datum : wrong Board)
scriptContext 16 = mkScriptContext_Move illegalMove2

-- Illegal Move : Initial hexagon is Empty (Datum : wrong Board)
scriptContext 17 = mkScriptContext_Move illegalMove3

-- Illegal Move : Initial hexagon is Blue (Datum : wrong Board)
scriptContext 18 = mkScriptContext_Move illegalMove4

-- Before Deadline (Succeed)
scriptContext 19 = mkScriptContext_Time currentTime2024Jan1

-- At Deadline (Succeed)
scriptContext 20 = mkScriptContext_Time $ currentTime2024Jan1 + turnDuration7h

-- After Deadline by 1 millisecond (Should Fail : After Deadline)
scriptContext 21 = mkScriptContext_Time $ currentTime2024Jan1 + turnDuration7h + turnDuration1ms

-- Missing registered NFT (Unauthorized access)
scriptContext 22 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           2       []   1
            , outputScript  (plift RunGameSC.address)   200     []  newGameInfo
            ] <> scriptContextConstantPart

-- unregistered NFT (Unauthorized access)
scriptContext 23 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           2       [identifierNFT_C]   1
            , outputScript  (plift RunGameSC.address)   200     []  newGameInfo
            , outputNFT     bobPaymentAddress identifierNFT_C
            ] <> scriptContextConstantPart

-- stolen playerA's NFT (Unauthorized access)
scriptContext 24 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           2       [identifierNFT_A]   1
            , outputScript  (plift RunGameSC.address)   200     []  newGameInfo
            , outputNFT     bobPaymentAddress identifierNFT_A
            ] <> scriptContextConstantPart

scriptContext _ = error "Undefined Script Context!"

----------------------------------------------- | Script Context Functions | -----------------------------------------------

mkScriptContext_Move :: Move -> ScriptContext
mkScriptContext_Move move = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  customGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart
            where
                customGameInfo = makeGameInfoFromMove move

mkScriptContext_Time :: POSIXTime -> ScriptContext
mkScriptContext_Time lowerBound = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  newGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> init scriptContextConstantPart <> [timeRange customValidRange]
            where
                customValidRange = Interval (LowerBound (Finite lowerBound) True) (UpperBound (PosInf) False)

