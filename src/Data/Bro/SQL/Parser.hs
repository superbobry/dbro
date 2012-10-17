{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Data.Bro.SQL.Parser
  ( statement
  ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Prelude hiding (takeWhile)

import Data.Text (Text)
#ifdef DEBUG
import Data.Functor ((<$))
import Control.Applicative (empty)

import Data.Attoparsec.Text.Parsec (Parser, choice, takeWhile, sepBy1,
                                    char, stringCI, skipSpace)
import Text.Parsec.Prim (getInput, setInput)
import qualified Data.Text.Read as TR
#else
import Data.Attoparsec.Text (Parser, choice, takeWhile, sepBy1, signed, double,
                            decimal, char, stringCI, skipSpace)
#endif

import Data.Bro.Types (TableName, TableSchema,
                       ColumnName, ColumnType(..), ColumnValue(..),
                       Statement(..))

signedDecimal :: Integral a => Parser a
signedDouble :: Parser Double

#ifdef DEBUG
getSetParser :: (Text -> Either String (a, Text)) -> Parser a
getSetParser f = do
    s <- getInput
    case f s of
        Right (n, s') -> n <$ setInput s'
        Left _         -> empty

decimal :: Integral a => Parser a
decimal = getSetParser TR.decimal

signedDecimal = getSetParser $ TR.signed TR.decimal
signedDouble = getSetParser $ TR.signed TR.double

#else
signedDecimal = signed decimal
signedDouble = signed double
#endif

statement :: Parser Statement
statement = choice [selectAll, createTable, insertInto]
            <* skipSpace
            <* char ';'
  where
    createTable = do
        tokens ["create", "table"]
        CreateTable <$> tableName <*> tableSchema

    insertInto = do
        tokens ["insert", "into"]
        table <- takeWhile isAlphaNum
        columns <- list1 columnName
        token "values"
        values <- list1 columnValue
        return $! InsertInto table (zip columns values)

    selectAll = do
        tokens ["select", "*", "from"]
        SelectAll <$> tableName

tableName :: Parser TableName
tableName = word

tableSchema :: Parser TableSchema
tableSchema = list1 $ do
    name <- columnName
    void $ skipSpace
    t    <- columnType
    return $ (name, t)

columnName :: Parser ColumnName
columnName = word

columnType :: Parser ColumnType
columnType =
    choice [ stringCI "int" *> pure IntegerColumn
           , stringCI "double" *> pure DoubleColumn
           , VarcharColumn <$> (token "varchar" *> decimal)
           ]

columnValue :: Parser ColumnValue
columnValue = choice [ IntegerValue <$> signedDecimal
                     , DoubleValue <$> signedDouble
                     , VarcharValue <$> word
                     ]

word :: Parser Text
word = takeWhile isAlphaNum
{-# INLINE word #-}

token :: Text -> Parser ()
token s = void $ stringCI s *> skipSpace
{-# INLINE token #-}

tokens :: [Text] -> Parser ()
tokens = mapM_ token
{-# INLINE tokens #-}

between :: Char -> Char -> Parser a -> Parser a
between open close p =
    (char open <* skipSpace) *> p <* (skipSpace *> char close)
{-# INLINE between #-}

list1 :: Parser a -> Parser [a]
list1 p = between '(' ')' $ p `sepBy1` (char ',' <* skipSpace)
{-# INLINE list1 #-}
