module Main (main) where

import PUtilities (writeScriptToFile)
import qualified InitialiseGameSC
import qualified RunGameSC
import qualified PlayerIdentifierMP
import qualified RefNFTManagerSC

main :: IO ()
main = do
    printLine >> writeScriptToFile InitialiseGameSC.script      "InitialiseGameSC"
    printLine >> writeScriptToFile RunGameSC.script             "RunGameSC"
    printLine >> writeScriptToFile PlayerIdentifierMP.script    "PlayerIdentifierMP"
    printLine >> writeScriptToFile RefNFTManagerSC.script       "RefNFTManagerSC"
    printLine

printLine :: IO ()
printLine = putStrLn "\ESC[90m__________________________________________________________________________________\ESC[0m\n"