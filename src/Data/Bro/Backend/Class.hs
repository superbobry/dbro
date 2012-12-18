{-# LANGUAGE NamedFieldPuns #-}

module Data.Bro.Backend.Class
  ( Backend(..)
  , Query(..)
  , fetchTable
  , withTable
  , transformRow
  ) where

import Control.Applicative ((<$>))
import Control.Monad.State (modify)
import Control.Monad.Error (throwError)

import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Condition (evalCondition)
import Data.Bro.Expr (evalExpr)
import Data.Bro.Monad (Bro)
import Data.Bro.Types (TableName, TableSchema, Table(..), Row(..), RowId,
                       ColumnName, ColumnValue, Expr, Projection(..),
                       Condition)

class Backend b where
    insertTable :: TableName -> TableSchema -> Bro BackendError b ()

    lookupTable :: TableName -> Bro BackendError b (Maybe Table)

    modifyTable :: TableName -> (Table -> Table) -> Bro BackendError b ()
    modifyTable = undefined

    modifyBackend :: (b -> b) -> Bro BackendError b ()
    modifyBackend = modify

    deleteTable :: TableName -> Bro BackendError b ()

class Backend b => Query b where
    selectAll :: TableName -> Bro BackendError b [Row]

    select :: TableName -> Projection -> Maybe Condition -> Bro BackendError b [Row]
    select name p c = withTable name $ \table -> do
        projectRows table p <$> case c of
            Just condition ->
                filterRows table condition <$> selectAll name
            Nothing -> selectAll name

    insertInto :: TableName -> Row -> Bro BackendError b RowId

    update :: TableName -> [(ColumnName, Expr)] -> (Maybe Condition) -> Bro BackendError b Int

fetchTable :: Backend b => TableName -> Bro BackendError b Table
fetchTable name = do
    res <- lookupTable name
    maybe (throwError TableDoesNotExist) return res
{-# INLINE fetchTable #-}

withTable :: Backend b
          => TableName
          -> (Table -> Bro BackendError b a)
          -> Bro BackendError b a
withTable name f = f =<< fetchTable name
{-# INLINE withTable #-}

transformRow :: Table -> [(ColumnName, Expr)] -> Row -> Row
transformRow (Table { tabSchema = (schema, _) }) es r@(Row { rowData }) =
    r { rowData = map transform pairs }
  where
    pairs :: [(ColumnName, ColumnValue)]
    pairs = zip names rowData

    names :: [ColumnName]
    names = map fst schema

    transform :: (ColumnName, ColumnValue) -> ColumnValue
    transform (name, v) = case lookup name es of
        Just e  -> evalExpr pairs e
        Nothing -> v

filterRows :: Table -> Condition -> [Row] -> [Row]
filterRows (Table { tabSchema = (schema, _) }) c = filter f where
  names :: [ColumnName]
  names = map fst schema

  f :: Row -> Bool
  f (Row { rowData }) = evalCondition (zip names rowData) c

projectRows :: Table -> Projection -> [Row] -> [Row]
projectRows _table (Projection []) = id
projectRows (Table { tabSchema = (schema, _) }) (Projection p) = map f where
  names :: [ColumnName]
  names = map fst schema

  f :: Row -> Row
  f r@(Row { rowData }) = r { rowData = map (evalExpr $ zip names rowData) p }
