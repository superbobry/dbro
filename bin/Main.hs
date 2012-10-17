module Main where

import Control.Monad (forever)
import Prelude hiding (getLine)

import Data.Attoparsec.Text (parseTest)
import Data.Text.IO (getLine)

import Data.Bro.Parser (statement)

main :: IO ()
main = forever $
       parseTest statement =<< getLine
