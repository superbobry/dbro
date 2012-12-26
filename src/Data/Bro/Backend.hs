{-# LANGUAGE NamedFieldPuns, BangPatterns #-}

module Data.Bro.Backend
  ( exec
  ) where

import Control.Applicative ((<$>), (<$))
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M

import Control.Monad.Error (throwError)
import Data.Default (def)

import Data.Bro.Backend.Class (Backend, Query(..), withTable)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Result (BackendResult(..))
import Data.Bro.Monad (Bro)
import Data.Bro.Types (Table(..), TableSchema, Row(..),
                       ColumnName, ColumnType(..), ColumnValue(..),
                       Statement(..))
import qualified Data.Bro.Backend.Class as Backend

exec :: (Query b, Backend b) => Statement -> Bro BackendError b (BackendResult b)
exec s = case s of
    CreateTable name schema -> Created <$ Backend.insertTable name schema
    Select name p c -> return . Selected $! Backend.select name p c
    InsertInto name columns values -> withTable name $ \Table { tabSchema } -> do
        remapped <- case columns of
            []    -> return values
            (_:_) -> remap tabSchema $ zip columns values
        Inserted <$> Backend.insertInto name (def { rowData = remapped })
    Update name exprs cond -> withTable name $ \_table ->
        Updated <$> Backend.update name exprs cond
    Delete name cond -> withTable name $ \_table ->
        Deleted <$> Backend.delete name cond
    CreateIndex index name columns ->
        Created <$ Backend.createIndex name index (map fst columns)

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
