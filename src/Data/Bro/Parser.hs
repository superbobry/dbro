{-# LANGUAGE OverloadedStrings #-}

module Data.Bro.Parser
  ( statement
  , columnValue
  ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), pure)
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Prelude hiding (takeWhile)

import Data.Text (Text)
import Data.Attoparsec.Text (Parser, Number(..), (<?>), choice, takeWhile,
                             sepBy1,
                             decimal, number, char, stringCI,
                             skipSpace)
import qualified Data.Text as T

import Data.Bro.Types (TableName, TableSchema,
                       ColumnName, ColumnType(..), ColumnValue(..),
                       Statement(..))

statement :: Parser Statement
statement = choice [selectAll, createTable, insertInto]
            <* skipSpace
            <* char ';'
  where
    createTable = "createTable" ?> do
        tokens ["create", "table"]
        CreateTable <$> tableName <*> tableSchema

    insertInto = "insertInto" ?> do
        tokens ["insert", "into"]
        table <- takeWhile isAlphaNum
        columns <- listOf1 columnName
        token "values"
        values <- listOf1 columnValue
        return $! InsertInto table (zip columns values)

    selectAll = "selectAll" ?> do
        tokens ["select", "*", "from"]
        SelectAll <$> tableName

tableName :: Parser TableName
tableName = "tableName" ?> word

tableSchema :: Parser TableSchema
tableSchema = listOf1 $ do
    name <- columnName <* skipSpace
    t    <- columnType
    return $ (name, t)

columnName :: Parser ColumnName
columnName = "columnName" ?> word

columnType :: Parser ColumnType
columnType =
    "columnType" ?>
    choice [ stringCI "int" *> pure IntegerColumn
           , stringCI "double" *> pure DoubleColumn
           , VarcharColumn <$> (token "varchar" *> between '(' ')' decimal)
           ]

columnValue :: Parser ColumnValue
columnValue = "columnValue" ?> varcharValue <|> numberValue where
  varcharValue :: Parser ColumnValue
  varcharValue = VarcharValue <$> between '"' '"' word

  numberValue :: Parser ColumnValue
  numberValue = do
      result <- number
      return $ case result of
          I i -> IntegerValue i
          D d -> DoubleValue d

word :: Parser Text
word = takeWhile isAlphaNum
{-# INLINE word #-}

token :: Text -> Parser ()
token s = T.unpack s ?> void (skipSpace *> stringCI s *> skipSpace)
{-# INLINE token #-}

tokens :: [Text] -> Parser ()
tokens = mapM_ token
{-# INLINE tokens #-}

between :: Char -> Char -> Parser a -> Parser a
between open close p =
    (char open <* skipSpace) *> p <* (skipSpace *> char close)
{-# INLINE between #-}

listOf1 :: Parser a -> Parser [a]
listOf1 p = between '(' ')' $ p `sepBy1` (char ',' <* skipSpace)
{-# INLINE listOf1 #-}

(?>) :: String -> Parser a -> Parser a
(?>) = flip (<?>)
{-# INLINE (?>) #-}
infix 0 ?>
