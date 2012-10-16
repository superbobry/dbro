{-# LANGUAGE CPP, StandaloneDeriving #-}

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

data ColumnValue = IntegerValue Int
                 | DoubleValue Double
                 | VarcharValue Text

type TableName = Text
type TableSchema = [(ColumnName, ColumnType)]

data Statement = CreateTable TableName TableSchema
               | InsertInto TableName [(ColumnName, ColumnValue)]
               | SelectAll TableName

#ifdef DEBUG
deriving instance Show ColumnType
deriving instance Show ColumnValue
deriving instance Show Statement
#endif
