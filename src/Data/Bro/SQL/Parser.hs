{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Data.Bro.SQL.Parser
  ( statement
  ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Prelude hiding (takeWhile)

import Data.Text (Text)
import Data.Attoparsec.Text (Parser, choice, takeWhile, sepBy1,
                             signed, decimal, double, char, stringCI,
                             skipSpace)

import Data.Bro.Types (TableName, TableSchema,
                       ColumnName, ColumnType(..), ColumnValue(..),
                       Statement(..))

statement :: Parser Statement
statement = choice [selectAll, createTable, insertInto]
            <* skipSpace
            <* char ';'
  where
    createTable = do
        void $ stringCI "create" <* skipSpace
        void $ stringCI "table" <* skipSpace
        CreateTable <$> tableName <*> tableSchema

    insertInto = do
        void $ stringCI "insert" <* skipSpace
        void $ stringCI "into" <* skipSpace
        table <- takeWhile isAlphaNum
        columns <- list1 columnName
        void $ stringCI "values"
        values <- list1 columnValue
        return $! InsertInto table (zip columns values)

    selectAll = do
        void $ stringCI "select" <* skipSpace
        void $ char '*' <* skipSpace
        void $ stringCI "from" <* skipSpace
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
           , VarcharColumn <$> (stringCI "varchar" *> skipSpace *> decimal)
           ]

columnValue :: Parser ColumnValue
columnValue = choice [ IntegerValue <$> signed decimal
                     , DoubleValue <$> signed double
                     , VarcharValue <$> word
                     ]

word :: Parser Text
word = takeWhile isAlphaNum

between :: Char -> Char -> Parser a -> Parser a
between open close p =
    (char open <* skipSpace) *> p <* (skipSpace *> char close)

list1 :: Parser a -> Parser [a]
list1 p = between '(' ')' $ p `sepBy1` (char ',' <* skipSpace)
