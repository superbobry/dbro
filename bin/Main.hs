{-# LANGUAGE CPP #-}

module Main where

import Control.Monad (forever)
import Prelude hiding (getLine)

#ifdef DEBUG
import Data.Attoparsec.Text.Parsec (parseOnly)
#else
import Data.Attoparsec.Text (parseOnly)
#endif
import Data.Text.IO (getLine)

import Data.Bro.SQL.Parser (statement)

main :: IO ()
main = forever $
    print . parseOnly statement =<< getLine
