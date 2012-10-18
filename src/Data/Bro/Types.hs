module Data.Bro.Types
  ( TableName
  , TableSchema
  , Table(..)
  , RowId
  , Row(..)
  , ColumnName
  , ColumnType(..)
  , ColumnValue(..)
  , Statement(..)
  ) where

import Data.Word (Word8)

import Data.Text (Text)

type RowId = Int

data Row = Row { rowId   :: Maybe RowId
               , rowData :: ![(ColumnName, ColumnValue)]
               } deriving (Eq, Show)

type ColumnName = Text
data ColumnType = IntegerColumn
                | DoubleColumn
                | VarcharColumn Word8
                deriving (Eq, Show)

data ColumnValue = IntegerValue Integer
                 | DoubleValue Double
                 | VarcharValue !Text
                 deriving (Eq, Show)

type TableName = Text
type TableSchema = [(ColumnName, ColumnType)]

data Table = Table { tabName   :: TableName
                   , tabSchema :: !TableSchema
                   }

data Statement = CreateTable TableName TableSchema
               | InsertInto TableName Row
               | SelectAll TableName
               deriving (Eq, Show)
