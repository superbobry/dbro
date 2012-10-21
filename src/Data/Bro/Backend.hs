{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Data.Bro.Backend
  ( Backend(..)
  , BackendError(..)

  , withTable
  , insertInto
  , selectAll
  , exec
  ) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)

import Data.Bro.Types (TableName, TableSchema, Table(..), Row(..), RowId, ColumnValue, 
                       Statement(..))

data BackendError = TableDoesNotExist
                  | TableAlreadyExists
                  deriving Show

data ExecResult = Create
                | Insert RowId
                | Select [(RowId, ColumnValue)]
                deriving Show

class Backend b where
    createTable :: b -> TableName -> TableSchema -> Either BackendError b

    lookupTable :: b -> TableName -> Maybe Table

    modifyTable :: b -> TableName -> (Table -> (a, Table)) -> Either BackendError (a, b)

    deleteTable :: b -> TableName -> Either BackendError b

exec :: Backend b => b -> Statement -> Either BackendError (b, ExecResult)
exec b (CreateTable name schema) = case createTable b name schema of
                                       Right newB -> Right (newB, Create)
                                       Left e -> Left e
--exec b (InsertInto name row) = (b, insertInto b name row >>= return . Insert
--exec b (SelectAll name) = selectAll b name >>= return . Select

withTable :: Backend b => b -> TableName -> (Table -> a) -> Maybe a
withTable b name f = f <$> lookupTable b name

insertInto :: Backend b => b -> TableName -> Row -> Either BackendError (RowId, b)
insertInto b name (Row { rowId = Nothing, .. }) =
    modifyTable b name $ \t@(Table { .. }) ->
        let t' = t { tabData = (tabCounter, map snd rowData) : tabData
                   , tabCounter = tabCounter + 1
                   }
        in (tabCounter, t')

selectAll :: Backend b => b -> TableName -> [Row]
selectAll b name = fromMaybe [] result where
  result = withTable b name $ \Table { .. } -> do
      (rowId, rowValues) <- tabData
      return $ Row { rowId = Just rowId
                   , rowData = zip (map fst tabSchema) rowValues
                   }
