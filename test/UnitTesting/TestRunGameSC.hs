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
import qualified InitialiseGameSC
import UtilityFxs (makeMove, unsafeMakeMove, botPlayGame, makeStartingBoard)
import PlutusLedgerApi.V1.Value (AssetClass)

------------------------------------------------------ | Unit Tests | ------------------------------------------------------

testAllConditions :: TestTree
testAllConditions = testGroup "Unit Testing : RunGameSC Validator"
    [ testGroup "RunGame : PlayTurn" (testElement RunGameSC.validator . getTestCase <$> conditions_PlayTurn)
    , testGroup "RunGame : GameOver" (testElement RunGameSC.validator . getTestCase <$> conditions_GameOver)
    , testGroup "RunGame : Draw"     (testElement RunGameSC.validator . getTestCase <$> conditions_Draw)
    , testGroup "RunGame : TimeOut"  (testElement RunGameSC.validator . getTestCase <$> conditions_TimeOut)
    ]

------------------------------------------------------ Conditions

allConditions :: [TestElement]
allConditions = concat [conditions_PlayTurn, conditions_GameOver, conditions_TimeOut]

conditions_PlayTurn, conditions_GameOver, conditions_TimeOut, conditions_Draw :: [TestElement]

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

conditions_GameOver =   [ Condition 11 " 1 - Game is Over"
                        , Condition 12 " 2 - Redeeming player is registered as a player in the game."
                        , Condition 13 " 3 - Redeeming player is the Winner."
                        , Condition 14 " 4 - Player's registered NFT is found in the input UTxOs."
                        ]

conditions_Draw     =   [ Condition 21 " 1 - Only 1 UTxO consumed from RunGameSC."
                        , Condition 22 " 2 - Game is Over"
                        , Condition 23 " 3 - True Draw"
                        , Condition 24 " 4 - Continuing Output: Own UTxO is sent to own smart contract."
                        , Condition 25 " 5 - Value is halved"
                        , Condition 26 " 6 - correct Datum: Only second player left"
                        , Condition 27 " 7 - Player's registered NFT is found in the input UTxOs."
                        , Condition 28 " 1 - Only one player left"
                        , Condition 29 " 2 - Player's registered NFT is found in the input UTxOs."
                        ]

conditions_TimeOut  =   [ Condition 31 " 1 - It is TimeOut"
                        , Condition 32 " 2 - Opponent claiming the win."
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
getArguments 01 1 = Just $ Right [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 01]
getArguments 01 2 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 02]
getArguments 02 1 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 03]
getArguments 02 2 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 04]
getArguments 03 1 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 05]
getArguments 04 1 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 06]
getArguments 04 2 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 07]
getArguments 04 3 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 08]
getArguments 05 1 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 09]
getArguments 05 2 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 10]
getArguments 06 1 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 11]
getArguments 07 1 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 12]
getArguments 08 1 = Just $ Right [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 13]
getArguments 08 2 = Just $ Right [toData initialGameInfo , toData (PlayTurn legalMove2)   , toData $ scriptContext 14]
getArguments 08 3 = Just $ Left  [toData initialGameInfo , toData (PlayTurn illegalMove1) , toData $ scriptContext 15]
getArguments 08 4 = Just $ Left  [toData initialGameInfo , toData (PlayTurn illegalMove2) , toData $ scriptContext 16]
getArguments 08 5 = Just $ Left  [toData initialGameInfo , toData (PlayTurn illegalMove3) , toData $ scriptContext 17]
getArguments 08 6 = Just $ Left  [toData initialGameInfo , toData (PlayTurn illegalMove4) , toData $ scriptContext 18]
getArguments 09 1 = Just $ Right [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 19]
getArguments 09 2 = Just $ Right [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 20]
getArguments 09 3 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 21]
getArguments 10 1 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 22]
getArguments 10 2 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 23]
getArguments 10 3 = Just $ Left  [toData initialGameInfo , toData (PlayTurn legalMove1)   , toData $ scriptContext 24]

getArguments 11 1 = Just $ Right [toData finalGameInfo   , toData (GameOver playerA) , toData $ scriptContext 102]
getArguments 11 2 = Just $ Right [toData finalGameInfo2  , toData (GameOver playerB) , toData $ scriptContext 101]
getArguments 11 3 = Just $ Right [toData finalGameInfo3  , toData (GameOver playerB) , toData $ scriptContext 101]
getArguments 11 4 = Just $ Left  [toData initialGameInfo , toData (GameOver playerA) , toData $ scriptContext 102]
getArguments 11 5 = Just $ Left  [toData initialGameInfo , toData (GameOver playerB) , toData $ scriptContext 101]
getArguments 12 1 = Just $ Left  [toData finalGameInfo   , toData (GameOver playerC) , toData $ scriptContext 103]
getArguments 12 2 = Just $ Left  [toData finalGameInfo   , toData (GameOver playerC) , toData $ scriptContext 104]
getArguments 13 1 = Just $ Left  [toData finalGameInfo   , toData (GameOver playerB) , toData $ scriptContext 101]
getArguments 13 2 = Just $ Left  [toData finalGameInfo2  , toData (GameOver playerA) , toData $ scriptContext 102]
getArguments 13 3 = Just $ Left  [toData finalGameInfo3  , toData (GameOver playerA) , toData $ scriptContext 102]
getArguments 14 1 = Just $ Left  [toData finalGameInfo   , toData (GameOver playerA) , toData $ scriptContext 105]

getArguments 21 1 = Just $ Right [toData finalGameInfo4  , toData Draw , toData $ scriptContext 201]
getArguments 21 2 = Just $ Left  [toData finalGameInfo4  , toData Draw , toData $ scriptContext 202]
getArguments 22 1 = Just $ Left  [toData initialGameInfo , toData Draw , toData $ scriptContext 201]
getArguments 23 1 = Just $ Left  [toData finalGameInfo   , toData Draw , toData $ scriptContext 201]
getArguments 24 1 = Just $ Left  [toData finalGameInfo4  , toData Draw , toData $ scriptContext 203]
getArguments 25 1 = Just $ Left  [toData finalGameInfo4  , toData Draw , toData $ scriptContext 204]
getArguments 25 2 = Just $ Left  [toData finalGameInfo4  , toData Draw , toData $ scriptContext 205]
getArguments 26 1 = Just $ Left  [toData finalGameInfo4  , toData Draw , toData $ scriptContext 206]
getArguments 26 2 = Just $ Left  [toData finalGameInfo4  , toData Draw , toData $ scriptContext 207]
getArguments 26 3 = Just $ Left  [toData finalGameInfo4  , toData Draw , toData $ scriptContext 208]
getArguments 26 4 = Just $ Left  [toData finalGameInfo4  , toData Draw , toData $ scriptContext 209]
getArguments 27 1 = Just $ Left  [toData finalGameInfo4  , toData Draw , toData $ scriptContext 210]
getArguments 28 1 = Just $ Right [toData finalGameInfo5  , toData Draw , toData $ scriptContext 212]
getArguments 28 2 = Just $ Left  [toData finalGameInfo4  , toData Draw , toData $ scriptContext 212]
getArguments 28 3 = Just $ Left  [toData finalGameInfo6  , toData Draw , toData $ scriptContext 212]
getArguments 28 4 = Just $ Left  [toData finalGameInfo7  , toData Draw , toData $ scriptContext 212]
getArguments 29 1 = Just $ Left  [toData finalGameInfo5  , toData Draw , toData $ scriptContext 211]

getArguments 31 1 = Just $ Left  [toData initialGameInfo , toData TimeOut , toData $ scriptContext 301]
getArguments 31 2 = Just $ Left  [toData initialGameInfo , toData TimeOut , toData $ scriptContext 302]
getArguments 31 3 = Just $ Right [toData initialGameInfo , toData TimeOut , toData $ scriptContext 303]
getArguments 32 1 = Just $ Left  [toData initialGameInfo , toData TimeOut , toData $ scriptContext 304]
getArguments 32 2 = Just $ Right [toData initialGameInfo , toData TimeOut , toData $ scriptContext 305]
getArguments 32 3 = Just $ Left  [toData initialGameInfo , toData TimeOut , toData $ scriptContext 306]

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

scriptContextConstantPart_PlayTurn :: [SpendingBuilder]
scriptContextConstantPart_PlayTurn =    [ inputScript   scriptH normalPlayTurn 200 [] 0
                                        , signedWith $ samplePubKeyHash "Bob"
                                        , txId $ makeTxId "Unique Random Text!"
                                        , fee (singleton adaSymbol adaToken 2)
                                        , withSpendingOutRefIdx 0
                                        , timeRange validRange
                                        ]

-- Datum : PlayerB's turn after he initiated the game (1st move)

initialGameInfo, initialGameInfo_A :: GameInfo
initialGameInfo     = GameInfo [playerA,playerB] turnDuration7h (Game playerB (currentTime2024Jan1+turnDuration7h) classicBoard_S9DC3)
initialGameInfo_A   = GameInfo [playerA,playerB] turnDuration7h (Game playerA (currentTime2024Jan1+turnDuration7h) classicBoard_S9DC3)

initialGameInfo_Size :: Integer -> GameInfo
initialGameInfo_Size s = GameInfo [playerA,playerB] turnDuration7h (Game playerA (currentTime2024Jan1+turnDuration7h) $ makeStartingBoard s [])

finalGameInfo, finalGameInfo2, finalGameInfo3, finalGameInfo4, finalGameInfo5, finalGameInfo6, finalGameInfo7 :: GameInfo
finalGameInfo  = botPlayGame initialGameInfo
finalGameInfo2 = botPlayGame initialGameInfo_A
finalGameInfo3 = botPlayGame $ initialGameInfo_Size 15
finalGameInfo4 = GameInfo [playerA,playerB] turnDuration7h (Game playerB (currentTime2024Jan1+turnDuration7h) drawBoard)
finalGameInfo5 = GameInfo [playerA] turnDuration7h (Game playerB (currentTime2024Jan1+turnDuration7h) drawBoard)
finalGameInfo6 = GameInfo [playerB] turnDuration7h (Game playerB (currentTime2024Jan1+turnDuration7h) drawBoard)
finalGameInfo7 = GameInfo [] turnDuration7h (Game playerB (currentTime2024Jan1+turnDuration7h) drawBoard)

drawBoard :: Board
drawBoard = foldr makeMove classicBoard_S5DC3 moves
    where
        moves = [ Move (Position 5 5) (Position 4 5)
                , Move (Position 1 3) (Position 2 4)
                , Move (Position 4 5) (Position 3 4)
                , Move (Position 5 5) (Position 4 5)
                , Move (Position 5 5) (Position 5 4)
                , Move (Position 3 1) (Position 4 2)
                , Move (Position 5 3) (Position 4 3)
                , Move (Position 2 2) (Position 3 3)
                , Move (Position 1 1) (Position 2 2)
                , Move (Position 1 1) (Position 2 1)
                , Move (Position 1 1) (Position 1 2)
                ]

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
            ] <> scriptContextConstantPart_PlayTurn

-- Player B trying to consume 2 UTxOs from RunGameSC (Double Satisfaction Attack)
scriptContext 2 = buildSpending checkPhase1 $ mconcat $
            [ inputScript   scriptH normalPlayTurn      200     []                  1
            , inputPubKey   bobPaymentAddress           2       [identifierNFT_B]   2
            , outputScript  (plift RunGameSC.address)   200     []  newGameInfo
            , outputValue   bobPaymentAddress           200     [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn

-- Player B trying to consume the UTxO without sending it to RunGameSC (A naive attack)
scriptContext 3 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           2       [identifierNFT_B]   1
            , outputValue   bobPaymentAddress           200     [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn

-- Player B trying to send the UTxO to a compromised RunGameSC address that has his own staking credential (Staking-Key-Control Attack)
scriptContext 4 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  compromisedRunGameSCAddress 200     []  newGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn

-- Output UTxO has less Lovelace (Illegal Value)
scriptContext 5 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   2       []  newGameInfo
            , outputValue   bobPaymentAddress           200     [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn

-- Output datum has missing player A (Datum : wrong Players)
scriptContext 6 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn
            where
                compromisedGameInfo = GameInfo [playerB] turnDuration7h gameStateS1

-- Output datum has players in different order (Datum : wrong Players)
scriptContext 7 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn
            where
                compromisedGameInfo = GameInfo [playerB,playerA] turnDuration7h gameStateS1

-- Output datum has different opponent NFT (Datum : wrong Players)
scriptContext 8 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn
            where
                compromisedGameInfo = GameInfo [playerA,playerY] turnDuration7h gameStateS1

-- Output datum has different turnDuration (Datum : wrong turnDuration)
scriptContext 9 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn
            where
                compromisedGameInfo = GameInfo [playerA,playerB] turnDuration1h gameStateS1

-- Output datum has different turnDuration & calculated deadline accordingly (Datum : wrong turnDuration)
scriptContext 10 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn
            where
                compromisedGameStateS1 = Game playerA (currentTime2024Jan1 + 2*turnDuration1h) newBoard
                compromisedGameInfo = GameInfo [playerA,playerB] turnDuration1h compromisedGameStateS1

-- PlayerA's turn (Datum : wrong player'sTurn)
scriptContext 11 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn
            where
                compromisedGameStateS1 = Game playerB (currentTime2024Jan1 + 2*turnDuration7h) newBoard
                compromisedGameInfo = GameInfo [playerA,playerB] turnDuration7h compromisedGameStateS1

-- compromised deadline (Datum : wrong deadline)
scriptContext 12 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  compromisedGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn
            where
                compromisedGameStateS1 = Game playerA (currentTime2024Jan1 + turnDuration7h + turnDuration1h) newBoard
                compromisedGameInfo = GameInfo [playerA,playerB] turnDuration7h compromisedGameStateS1

-- Legal Move : Distance = 1 (Datum : correct Board)
scriptContext 13 = mkPlayTurnScriptContext_Move legalMove1

-- Legal Move : Distance = 2 (Datum : correct Board)
scriptContext 14 = mkPlayTurnScriptContext_Move legalMove2

-- Illegal Move : Distance = 3 (Datum : wrong Board)
scriptContext 15 = mkPlayTurnScriptContext_Move illegalMove1

-- Illegal Move : Initial Position does not exist (Datum : wrong Board)
scriptContext 16 = mkPlayTurnScriptContext_Move illegalMove2

-- Illegal Move : Initial hexagon is Empty (Datum : wrong Board)
scriptContext 17 = mkPlayTurnScriptContext_Move illegalMove3

-- Illegal Move : Initial hexagon is Blue (Datum : wrong Board)
scriptContext 18 = mkPlayTurnScriptContext_Move illegalMove4

-- Before Deadline (Succeed)
scriptContext 19 = mkPlayTurnScriptContext_Time currentTime2024Jan1

-- At Deadline (Succeed)
scriptContext 20 = mkPlayTurnScriptContext_Time $ currentTime2024Jan1 + turnDuration7h

-- After Deadline by 1 millisecond (Should Fail : After Deadline)
scriptContext 21 = mkPlayTurnScriptContext_Time $ currentTime2024Jan1 + turnDuration7h + turnDuration1ms

-- Missing registered NFT (Unauthorized access)
scriptContext 22 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           2       []   1
            , outputScript  (plift RunGameSC.address)   200     []  newGameInfo
            ] <> scriptContextConstantPart_PlayTurn

-- unregistered NFT (Unauthorized access)
scriptContext 23 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           2       [identifierNFT_C]   1
            , outputScript  (plift RunGameSC.address)   200     []  newGameInfo
            , outputNFT     bobPaymentAddress identifierNFT_C
            ] <> scriptContextConstantPart_PlayTurn

-- stolen playerA's NFT (Unauthorized access)
scriptContext 24 = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           2       [identifierNFT_A]   1
            , outputScript  (plift RunGameSC.address)   200     []  newGameInfo
            , outputNFT     bobPaymentAddress identifierNFT_A
            ] <> scriptContextConstantPart_PlayTurn

-- Player B (Bob) ending the game by consuming the UTxO at RunGameSC
-- Success or failure (depending on Game Info in the datum - If he is the winner or not)
scriptContext 101 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (GameOver playerB)  200     []                  0
            , inputPubKey   bobPaymentAddress           2       [identifierNFT_B]   1
            , outputValue   bobPaymentAddress           200     [identifierNFT_B]
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            ]

-- Player A (Alice) ending the game by consuming the UTxO at RunGameSC
-- Success or failure (depending on Game Info in the datum - If she is the winner or not)
scriptContext 102 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (GameOver playerA)  200     []                  0
            , inputPubKey   alicePaymentAddress         2       [identifierNFT_A]   1
            , outputValue   alicePaymentAddress         200     [identifierNFT_A]
            , signedWith $ samplePubKeyHash "Alice"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            ]

-- Player C trying to end the game by consuming the UTxO at RunGameSC (unregistered player)
-- While holding a registered NFT.
scriptContext 103 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (GameOver playerC)  200     []                  0
            , inputPubKey   alicePaymentAddress         2       [identifierNFT_A]   1
            , outputValue   alicePaymentAddress         200     [identifierNFT_A]
            , signedWith $ samplePubKeyHash "Alice"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            ]

-- Player C trying to end the game by consuming the UTxO at RunGameSC (unregistered player)
-- While holding a unregistered NFT.
scriptContext 104 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (GameOver playerC)  200     []                  0
            , inputPubKey   alicePaymentAddress         2       [identifierNFT_C]   1
            , outputValue   alicePaymentAddress         200     [identifierNFT_C]
            , signedWith $ samplePubKeyHash "Alice"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            ]

-- Player A (Alice) ending the game by consuming the UTxO at RunGameSC
-- Missing registered NFT (Unauthorized access)
scriptContext 105 = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH (GameOver playerA)  200     []                  0
            , inputPubKey   alicePaymentAddress         2       []   1
            , outputValue   alicePaymentAddress         200     []
            , signedWith $ samplePubKeyHash "Alice"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            ]

-- Player B (Bob) ending the game by consuming the UTxO at RunGameSC
-- Only 1 UTxO consumed from RunGameSC (Success case)
scriptContext 201 = buildSpending checkPhase1 $ spendingBuilder_DrawA (plift RunGameSC.address) [playerA] 100 identifierNFT_B

-- Player B trying to consume 2 UTxOs from RunGameSC (Double Satisfaction Attack)
scriptContext 202 = buildSpending checkPhase1 $ mconcat $
            [ inputScript   scriptH Draw            200     []  2
            , outputValue   bobPaymentAddress       200     []
            ] <> [spendingBuilder_DrawA (plift RunGameSC.address) [playerA] 100 identifierNFT_B]

-- Player B trying to send the UTxO to a different smart-contract
scriptContext 203 = buildSpending checkPhase1 $ spendingBuilder_DrawA (plift InitialiseGameSC.address) [playerA] 100 identifierNFT_B

-- Player B trying to take more than half of the value
scriptContext 204 = buildSpending checkPhase1 $ spendingBuilder_DrawA (plift RunGameSC.address) [playerA] 101 identifierNFT_B

-- Player B trying to take less than half of the value
scriptContext 205 = buildSpending checkPhase1 $ spendingBuilder_DrawA (plift RunGameSC.address) [playerA] 99 identifierNFT_B

-- Compromised Output Datum (Remaining Player/s to claim)
scriptContext 206 = buildSpending checkPhase1 $ spendingBuilder_DrawA (plift RunGameSC.address) [playerA,playerB] 100 identifierNFT_B
scriptContext 207 = buildSpending checkPhase1 $ spendingBuilder_DrawA (plift RunGameSC.address) [playerA,playerA] 100 identifierNFT_B
scriptContext 208 = buildSpending checkPhase1 $ spendingBuilder_DrawA (plift RunGameSC.address) [playerB] 100 identifierNFT_B
scriptContext 209 = buildSpending checkPhase1 $ spendingBuilder_DrawA (plift RunGameSC.address) [] 100 identifierNFT_B

-- Unauthorized user trying to claim
scriptContext 210 = buildSpending checkPhase1 $ spendingBuilder_DrawA (plift RunGameSC.address) [playerA] 100 identifierNFT_A
scriptContext 211 = buildSpending checkPhase1 $ spendingBuilder_DrawB identifierNFT_B

-- Player A claiming the rest (Success case)
scriptContext 212 = buildSpending checkPhase1 $ spendingBuilder_DrawB identifierNFT_A

-- Player A (Alice) ending the game before deadline has reached (Should Fail : Deadline not passed yet)
scriptContext 301 = mkTimeOutScriptContext [identifierNFT_A] $ currentTime2024Jan1 + turnDuration7h - turnDuration1ms

-- Player A (Alice) ending the game at deadline has reached (Should Fail : Deadline not passed yet)
scriptContext 302 = mkTimeOutScriptContext [identifierNFT_A] $ currentTime2024Jan1 + turnDuration7h

-- Player A (Alice) ending the game after deadline has reached (Succeed)
scriptContext 303 = mkTimeOutScriptContext [identifierNFT_A] $ currentTime2024Jan1 + turnDuration7h + turnDuration1ms

-- Missing registered NFT (Unauthorized access)
scriptContext 304 = mkTimeOutScriptContext [] $ currentTime2024Jan1 + turnDuration7h + turnDuration1ms

-- Opponent player's NFT (Authorized access)
scriptContext 305 = mkTimeOutScriptContext [identifierNFT_A] $ currentTime2024Jan1 + turnDuration7h + turnDuration1ms

-- Current player's NFT (Unauthorized access)
scriptContext 306 = mkTimeOutScriptContext [identifierNFT_B] $ currentTime2024Jan1 + turnDuration7h + turnDuration1ms

scriptContext _ = error "Undefined Script Context!"

----------------------------------------------- | Script Context Functions | -----------------------------------------------

mkPlayTurnScriptContext_Move :: Move -> ScriptContext
mkPlayTurnScriptContext_Move move = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  customGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> scriptContextConstantPart_PlayTurn
            where
                customGameInfo = makeGameInfoFromMove move

mkPlayTurnScriptContext_Time :: POSIXTime -> ScriptContext
mkPlayTurnScriptContext_Time lowerBound = buildSpending checkPhase1 $ mconcat $
            [ inputPubKey   bobPaymentAddress           4       [identifierNFT_B]   1
            , outputScript  (plift RunGameSC.address)   200     []  newGameInfo
            , outputValue   bobPaymentAddress           2       [identifierNFT_B]
            ] <> init scriptContextConstantPart_PlayTurn <> [timeRange customValidRange]
            where
                customValidRange = Interval (LowerBound (Finite lowerBound) True) (UpperBound (PosInf) False)

mkTimeOutScriptContext :: [AssetClass] -> POSIXTime -> ScriptContext
mkTimeOutScriptContext nfts lowerBound = buildSpending checkPhase1 $ mconcat
            [ inputScript   scriptH TimeOut     200     []      0
            , inputPubKey   alicePaymentAddress 2       nfts    1
            , outputValue   alicePaymentAddress 200     nfts
            , signedWith $ samplePubKeyHash "Alice"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            , timeRange $ Interval (LowerBound (Finite lowerBound) True) (UpperBound (PosInf) False)
            ]

spendingBuilder_DrawA :: Address -> [Player] -> Integer -> AssetClass -> SpendingBuilder
spendingBuilder_DrawA output remainingPlayers valueToClaim identifierNFT = mconcat
            [ inputScript   scriptH Draw        200                     []                  0
            , inputPubKey   bobPaymentAddress   2                       [identifierNFT]     1
            , outputValue   bobPaymentAddress   valueToClaim            [identifierNFT]
            , outputScript  output              (200 - valueToClaim)    []                  newDrawGameInfo
            , signedWith $ samplePubKeyHash "Bob"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            ]
            where
                newDrawGameInfo = GameInfo remainingPlayers turnDuration7h (Game playerB (currentTime2024Jan1+turnDuration7h) drawBoard)

spendingBuilder_DrawB :: AssetClass -> SpendingBuilder
spendingBuilder_DrawB identifierNFT = mconcat
            [ inputScript   scriptH Draw        100         []                  0
            , inputPubKey   bobPaymentAddress   2           [identifierNFT]     1
            , outputValue   bobPaymentAddress   100         [identifierNFT]
            , signedWith $ samplePubKeyHash "Alice"
            , txId $ makeTxId "Unique Random Text!"
            , fee (singleton adaSymbol adaToken 2)
            , withSpendingOutRefIdx 0
            ]
