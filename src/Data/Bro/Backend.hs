module Data.Bro.Backend
  ( Backend(..)
  ) where

import Data.Bro.Types (TableName, TableSchema, Table, RowId, Row)

class Backend a where
    createTable :: a -> TableName -> TableSchema -> Maybe Table

    insertInto  :: a -> TableName -> Row -> RowId

    selectAll   :: a -> TableName -> [Row]
