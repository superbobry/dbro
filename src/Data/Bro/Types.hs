{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, NamedFieldPuns #-}

module Data.Bro.Types
  ( TableName
  , TableSchema
  , Table(..)
  , RowId
  , Row(..)
  , ColumnName
  , ColumnType(..)
  , ColumnValue(..)
  , Projection(..)
  , Condition(..)
  , Expr(..)
  , Statement(..)
  , Simple(..)
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary(..), get, put)
import Data.Maybe (isNothing)
import Data.Int (Int32)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as S

import Data.Default (Default(..))

type RowId = Int

data Row = Row { rowId   :: Maybe RowId
               , rowData :: ![ColumnValue]
               } deriving (Eq, Show)

instance Binary Row where
    put (Row { rowId }) | isNothing rowId = fail "Row is missing an id"
    put (Row { .. }) = put rowId >> put rowData

    get = Row <$> get <*> get

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
        _   -> error $ "Not a valid column type: " ++ [tag]

data ColumnValue = IntegerValue {-# UNPACK #-} !Int32
                 | DoubleValue  {-# UNPACK #-} !Double
                 | VarcharValue S.ByteString
    deriving (Eq, Show)

instance Binary ColumnValue where
    put (IntegerValue v) = put 'i' >> put v
    put (DoubleValue d)  = put 'd' >> put d
    put (VarcharValue s) = put 'v' >> put s

    get = get >>= \tag -> case tag of
        'i' -> IntegerValue <$> get
        'd' -> DoubleValue <$> get
        'v' -> VarcharValue <$> get
        _   -> error $ "Not a valid column value: " ++ [tag]

type TableName = S.ByteString
type TableSchema = [(ColumnName, ColumnType)]

data Table = Table { tabName    :: TableName
                   , tabSchema  :: !(TableSchema, Int)
                   , tabCounter :: {-# UNPACK #-} !RowId
                   , tabSize    :: {-# UNPACK #-} !Int
                   } deriving (Eq, Show)

instance Binary Table where
    put (Table { .. }) =
        put tabName >> put tabSchema >> put tabCounter >> put tabSize

    get = Table <$> get <*> get <*> get <*> get

instance Default Table where
    def = Table { tabName = S.empty
                , tabSchema = ([], 0)
                , tabCounter = 1
                , tabSize = 0
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

newtype Projection = Projection [(ColumnName, Expr)] deriving (Eq, Show)

data Statement = CreateTable TableName TableSchema
               | InsertInto TableName ![(ColumnName, ColumnValue)]
               | Select TableName Projection (Maybe Condition)
               | Update TableName ![(ColumnName, Expr)] (Maybe Condition)
               | Delete TableName (Maybe Condition)
    deriving (Eq, Show)

class Simple a where
    simplify :: a -> a


instance Simple Expr where
    simplify (Negate (Const (DoubleValue d))) = Const (DoubleValue $ -d)
    simplify (Negate (Const (IntegerValue i))) = Const (IntegerValue $ -i)
    simplify (Negate (Negate e)) = simplify e
    simplify e = e

instance Simple Projection where
    simplify (Projection es) = Projection $ map simplify es

instance Simple Condition where
    simplify (Equals n e) = Equals n (simplify e)
    simplify (NotEquals n e) = NotEquals n (simplify e)
    simplify (GreaterThan n e) = GreaterThan n (simplify e)
    simplify (LowerThan n e) = LowerThan n (simplify e)
    simplify (Or e1 e2) = Or (simplify e1) (simplify e2)
    simplify (And e1 e2) = And (simplify e1) (simplify e2)

instance Simple Statement where
    simplify (Select n p c) = Select n (simplify p) (simplify <$> c)
    simplify (Update n bs c) =
        Update n [(name, simplify e) | (name, e) <- bs] (simplify <$> c)
    simplify (Delete n c) = Delete n (simplify <$> c)
    simplify s = s
