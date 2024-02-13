module Main (main) where
import Testing.Transactions (startGame, cancelGame)

main :: IO ()
main = do
    startGame
    cancelGame
