{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Bro.Types
  ( Ref(..)
  , TableName
  , TableSchema
  , Table(..)
  , RowId
  , Row(..)
  , ColumnName
  , ColumnType(..)
  , ColumnValue(..)
  , Statement(..)
  ) where

import Control.Applicative ((<$>))
import Data.Word (Word8)
import Data.Binary (Binary(..), get, put)

import qualified Data.ByteString.Char8 as S

newtype Ref a = Ref Int deriving (Eq, Ord, Show)

type RowId = Int

data Row = Row { rowId   :: Maybe RowId
               , rowData :: ![ColumnValue]
               } deriving (Eq, Show)

type ColumnName = S.ByteString
data ColumnType = IntegerColumn
                | DoubleColumn
                | VarcharColumn Word8
                deriving (Eq, Show)

instance Binary ColumnType where
    put IntegerColumn = put 'i'
    put DoubleColumn  = put 'd'
    put (VarcharColumn l) = put 'v' >> put l

    get = get >>= \tag -> case tag of
        'i' -> return IntegerColumn
        'd' -> return DoubleColumn
        'v' -> VarcharColumn <$> get
        _   -> error "Not a valid column type"

data ColumnValue = IntegerValue Integer
                 | DoubleValue Double
                 | VarcharValue S.ByteString
                 deriving (Eq, Show)

instance Binary ColumnValue where
    put (IntegerValue v) = put 'i' >> put v
    put (DoubleValue d)  = put 'd' >> put d
    put (VarcharValue t) = put 'v' >> put t

    get = get >>= \tag -> case tag of
        'i' -> IntegerValue <$> get
        'd' -> DoubleValue <$> get
        'v' -> VarcharValue <$> get
        _   -> error "Not a valid column value"

type TableName = S.ByteString
type TableSchema = [(ColumnName, ColumnType)]

data Table = Table { tabName    :: TableName
                   , tabSchema  :: !TableSchema
                   , tabData    :: ![Row]
                   , tabCounter :: RowId
                   } deriving (Eq, Show)

data Statement = CreateTable TableName TableSchema
               | InsertInto TableName ![(ColumnName, ColumnValue)]
               | SelectAll TableName
               deriving (Eq, Show)
