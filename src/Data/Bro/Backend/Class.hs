{-# LANGUAGE NamedFieldPuns #-}

module Data.Bro.Backend.Class
  ( Backend(..)
  , Query(..)
  , fetchTable
  , withTable
  , updateRow
  ) where

import Control.Monad.State (modify)
import Control.Monad.Error (throwError)
import Control.Monad.Trans (lift)
import Data.Conduit (Source, Conduit, ($=))
import qualified Data.Conduit.List as CL

import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Condition (evalCondition)
import Data.Bro.Expr (evalExpr)
import Data.Bro.Monad (Bro)
import Data.Bro.Types (TableName, TableSchema, Table(..), Row(..), RowId,
                       ColumnName, ColumnValue, Expr, Projection(..),
                       Condition, IndexName)

class Backend b where
    insertTable :: TableName -> TableSchema -> Bro BackendError b ()

    lookupTable :: TableName -> Bro BackendError b (Maybe Table)

    modifyTable :: TableName -> (Table -> Table) -> Bro BackendError b ()
    modifyTable = undefined

    modifyBackend :: (b -> b) -> Bro BackendError b ()
    modifyBackend = modify

    deleteTable :: TableName -> Bro BackendError b ()

class Backend b => Query b where
    selectAll :: Table -> Maybe Condition -> Source (Bro BackendError b) Row

    select :: Table -> Projection -> Maybe Condition -> Source (Bro BackendError b) Row
    select table p c =
        selectAll table c $= projectRows table p $= filterRows table c

    insertInto :: Table -> Row -> Bro BackendError b RowId

    update :: Table -> [(ColumnName, Expr)] -> Maybe Condition -> Bro BackendError b Int

    delete :: Table -> Maybe Condition -> Bro BackendError b Int

    createIndex :: Table -> IndexName -> [ColumnName] -> Bro BackendError b ()

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

updateRow :: Table -> [(ColumnName, Expr)] -> Row -> Row
updateRow (Table { tabSchema }) es r@(Row { rowData }) =
    r { rowData = map f pairs }
  where
    pairs :: [(ColumnName, ColumnValue)]
    pairs = zip names rowData

    names :: [ColumnName]
    names = map fst tabSchema

    f :: (ColumnName, ColumnValue) -> ColumnValue
    f (name, v) = case lookup name es of
        Just e  -> evalExpr pairs e
        Nothing -> v

filterRows :: Monad m => Table -> Maybe Condition -> Conduit Row m Row
filterRows _table Nothing = CL.map id
filterRows (Table { tabSchema }) (Just c) = CL.filter f where
  names :: [ColumnName]
  names = map fst tabSchema

  f :: Row -> Bool
  f (Row { rowData }) = evalCondition (zip names rowData) c

projectRows :: Monad m => Table -> Projection -> Conduit Row m Row
projectRows _table (Projection []) = CL.map id
projectRows (Table { tabSchema }) (Projection p) = CL.map f where
  names :: [ColumnName]
  names = map fst tabSchema

  f :: Row -> Row
  f r@(Row { rowData }) = r { rowData = map (evalExpr $ zip names rowData) p }
