{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, NamedFieldPuns #-}

module Data.Bro.Types
  ( TableName
  , TableSchema
  , TableIndex
  , IndexName
  , Table(..)
  , RowId
  , Row(..)
  , ColumnName
  , ColumnType(..)
  , ColumnValue(..)
  , Projection(..)
  , Condition(..)
  , Expr(..)
  , Direction(..)
  , Statement(..)
  , Range
  , RangeValue(..)
  , isIntegral
  , toIntegral
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Serialize (Serialize(..), get, put)
import Data.Maybe (isNothing)
import Data.Int (Int32)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as S

import Data.Default (Default(..))

type RowId = Int32

data Row = Row { rowId        :: Maybe RowId
               , rowData      :: ![ColumnValue]
               , rowIsDeleted :: Bool
               } deriving (Eq, Show)

instance Default Row where
    def = Row { rowId = Nothing
              , rowData = []
              , rowIsDeleted = False
              }

instance Serialize Row where
    put (Row { rowId }) | isNothing rowId = fail "Row is missing an id"
    put (Row { .. }) = put rowId >> put rowData >> put rowIsDeleted

    get = Row <$> get <*> get <*> get

type ColumnName = S.ByteString
data ColumnType = IntegerColumn
                | DoubleColumn
                | VarcharColumn Word8
    deriving (Eq, Show)

instance Serialize ColumnType where
    put IntegerColumn = put 'i'
    put DoubleColumn  = put 'd'
    put (VarcharColumn l) = put 'v' >> put l

    get = get >>= \tag -> case tag of
        'i' -> return IntegerColumn
        'd' -> return DoubleColumn
        'v' -> VarcharColumn <$> get
        _   -> fail $ "Not a valid column type: " ++ [tag]

data ColumnValue = IntegerValue {-# UNPACK #-} !Int32
                 | DoubleValue  {-# UNPACK #-} !Double
                 | VarcharValue S.ByteString
    deriving (Eq, Ord)

instance Show ColumnValue where
    show (IntegerValue i) = show i
    show (DoubleValue d) = show d
    show (VarcharValue s) = show s

instance Serialize ColumnValue where
    put (IntegerValue v) = put 'i' >> put v
    put (DoubleValue d)  = put 'd' >> put d
    put (VarcharValue s) = put 'v' >> put s

    get = get >>= \tag -> case tag of
        'i' -> IntegerValue <$> get
        'd' -> DoubleValue <$> get
        'v' -> VarcharValue <$> get
        _   -> fail $ "Not a valid column value: " ++ [tag]

type TableName = S.ByteString
type IndexName = S.ByteString
type TableSchema = [(ColumnName, ColumnType)]
type TableIndex = [(ColumnName, IndexName)]

data Table = Table { tabName    :: TableName
                   , tabCounter :: {-# UNPACK #-} !RowId
                   , tabSchema  :: !TableSchema
                   , tabRowSize :: {-# UNPACK #-} !Int32
                   , tabSize    :: {-# UNPACK #-} !Int32
                   , tabIndex   :: !TableIndex
                   } deriving (Eq, Show)

instance Serialize Table where
    put (Table { .. }) = do
        put tabName
        put tabCounter
        put tabSchema
        put tabRowSize
        put tabSize
        put tabIndex

    get = Table <$> get <*> get <*> get <*> get <*> get <*> get

instance Default Table where
    def = Table { tabName = S.empty
                , tabCounter = 1
                , tabSchema = []
                , tabRowSize = 0
                , tabSize = 0
                , tabIndex = []
                }

-- FIXME(Sergei): add comparisons here!
data Expr = Const ColumnValue
          | Field ColumnName
          | Negate Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
    deriving (Eq, Show)

data Condition = Equals ColumnName Expr
               | NotEquals ColumnName Expr
               | GreaterThan ColumnName Expr
               | LowerThan ColumnName Expr
               | Or Condition Condition
               | And Condition Condition
    deriving (Eq, Show)

newtype Projection = Projection [Expr] deriving (Eq, Show)

data Direction = Asc | Desc deriving (Eq, Show)

data Statement = CreateTable TableName TableSchema
               | InsertInto TableName ![ColumnName] ![ColumnValue]
               | Select TableName Projection (Maybe Condition)
               | Update TableName ![(ColumnName, Expr)] (Maybe Condition)
               | Delete TableName (Maybe Condition)
               | CreateIndex IndexName TableName ![(ColumnName, Direction)]
    deriving (Eq, Show)

data RangeValue = NumericRange Int32 | MinusInf | PlusInf deriving (Eq, Show)

type Range = [(RangeValue, RangeValue)]

isIntegral :: ColumnType -> Bool
isIntegral IntegerColumn = True
isIntegral _ = False

toIntegral :: ColumnValue -> Int32
toIntegral (IntegerValue i) = i
toIntegral _ = error "Not an integer value"
