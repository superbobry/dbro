module Data.Bro.Types
  ( TableName
  , TableSchema
  , ColumnName
  , ColumnType(..)
  , ColumnValue(..)
  , Statement(..)
  ) where

import Data.Word (Word8)

import Data.Text (Text)

type ColumnName = Text
data ColumnType = IntegerColumn
                | DoubleColumn
                | VarcharColumn Word8
                deriving (Eq, Show)

data ColumnValue = IntegerValue Integer
                 | DoubleValue Double
                 | VarcharValue Text
                 deriving (Eq, Show)

type TableName = Text
type TableSchema = [(ColumnName, ColumnType)]

data Statement = CreateTable TableName TableSchema
               | InsertInto TableName [(ColumnName, ColumnValue)]
               | SelectAll TableName
               deriving (Eq, Show)
