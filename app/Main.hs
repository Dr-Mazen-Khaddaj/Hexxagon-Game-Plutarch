{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
