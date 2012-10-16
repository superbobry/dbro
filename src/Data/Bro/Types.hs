{-# LANGUAGE CPP, ExistentialQuantification, StandaloneDeriving #-}

module Data.Bro.Types
  ( TableName
  , ColumnName
  , ColumnType(..)
  , Statement(..)
  , BroValue
  ) where

import Data.Word (Word8)

import Data.Text (Text)

type TableName = Text

type ColumnName = Text
data ColumnType = IntegerColumn
                | DoubleColumn
                | VarcharColumn Word8

#ifdef DEBUG
class Show a => BroValue a where
#else
class BroValue a where
#endif

data Statement = CreateTable TableName [(ColumnName, ColumnType)]
               | forall a. BroValue a => InsertInto TableName [(ColumnName, a)]
               | SelectAll TableName

#ifdef DEBUG
deriving instance Show ColumnType
deriving instance Show Statement
#endif
