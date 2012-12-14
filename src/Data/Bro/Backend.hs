{-# LANGUAGE NamedFieldPuns, BangPatterns #-}

module Data.Bro.Backend
  ( exec
  ) where

import Control.Applicative ((<$>), (<$))
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M

import Control.Monad.Error (throwError)

import Data.Bro.Backend.Class (Backend, Query(..), withTable)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Result (BackendResult(..))
import Data.Bro.Monad (Bro)
import Data.Bro.Types (Table(..), TableSchema, Row(..),
                       ColumnName, ColumnType(..), ColumnValue(..),
                       Projection(..), Statement(..))
import qualified Data.Bro.Backend.Class as Backend

exec :: (Query b, Backend b) => Statement -> Bro BackendError b BackendResult
exec s = case s of
    CreateTable name schema -> Created <$ Backend.insertTable name schema
    -- FIXME(Sergei): this is only a special case, update to a more generic
    -- version!
    Select name (Projection []) Nothing -> withTable name $ \table ->
        Selected table <$> Backend.selectAll name
    InsertInto name pairs -> withTable name $ \Table { tabSchema } -> do
        remapped <- remap (fst tabSchema) pairs
        let row = Row { rowId = Nothing, rowData = remapped }
        Inserted <$> Backend.insertInto name row

remap :: Backend b
      => TableSchema
      -> [(ColumnName, ColumnValue)]
      -> Bro BackendError b [ColumnValue]
remap schema = go schema [] . M.fromList where
  go ((name, t):rest) !acc m = case M.lookup name m of
      Just value | value `matches` t -> go rest (value:acc) m
      Just value -> throwError $ ColumnTypeMismatch value t
      Nothing    -> throwError $ ColumnValueMissing name
  go [] acc _m = return $! reverse acc

matches :: ColumnValue -> ColumnType -> Bool
matches (IntegerValue _v) IntegerColumn = True
matches (DoubleValue _d) DoubleColumn = True
matches (VarcharValue s) (VarcharColumn l) =
    -- FIXME(Sergei): make this a separate case, InvalidLengthSomething?
    S.length s <= fromIntegral l
matches _value _type = False
