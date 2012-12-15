module Data.Bro.Backend.Class
  ( Backend(..)
  , Query(..)
  , fetchTable
  , withTable
  , transformRow
  ) where

import Control.Monad.State (modify)
import Control.Monad.Error (throwError)

import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Monad (Bro)
import Data.Bro.Types 	(TableName, TableSchema, Table, Row, RowId,
						ColumnName, Expr, Condition)

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

    insertInto :: TableName -> Row -> Bro BackendError b RowId

    updateTable :: TableName -> [(ColumnName, Expr)] -> (Maybe Condition) -> Bro BackendError b Int

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

--dummy row transform function
transformRow :: [(ColumnName, Expr)] -> Row -> Row
transformRow _ r = r
