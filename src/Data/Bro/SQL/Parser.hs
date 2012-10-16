{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Data.Bro.SQL.Parser
  ( statement
  ) where

import Control.Applicative ((<|>), (<$>), (<*))
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Prelude hiding (takeWhile)

import Data.Attoparsec.Text (Parser,
                             takeWhile,
                             char, stringCI, skipSpace)

import Data.Bro.Types (Statement(..))

statement :: Parser Statement
statement =
    (selectAll <|> createTable <|> insertInto) <* skipSpace <* char ';'
  where
    createTable = return undefined

    insertInto = return undefined

    selectAll = do
        void $ stringCI "select" <* skipSpace
        void $ char '*' <* skipSpace
        void $ stringCI "from" <* skipSpace
        SelectAll <$> takeWhile isAlphaNum
