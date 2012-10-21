
module Main where

import Prelude hiding (getLine)

import Data.Attoparsec.Text (parseOnly)
import Data.Text.IO (getLine)

import Data.Bro.Parser (statement)
import Data.Bro.Backend (Backend(..), exec)
import Data.Bro.Backend.Memory (makeMemoryBackend)

mainLoop :: Backend b => b -> IO ()
mainLoop b = do
             input <- getLine
             case parseOnly statement input of
                Left e 	-> putStrLn ("ParseError: " ++ e) >> mainLoop b
                Right s -> case exec b s of 
                               Left be -> putStrLn ("Error: " ++ show be) >> mainLoop b
                               Right (newB, res) -> putStrLn ("OK: " ++ show res) >> mainLoop newB

main :: IO ()
main = mainLoop makeMemoryBackend
