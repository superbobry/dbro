{-# LANGUAGE RecordWildCards, NamedFieldPuns, TupleSections #-}

module Data.Bro.Backend
  ( Backend(..)
  , BackendError(..)
  , withTable
  , insertInto
  , selectAll
  , exec
  ) where

import Control.Applicative ((<$>))

import Data.Bro.Types (TableName, TableSchema, Table(..), Row(..), RowId,
                       Statement(..))

data BackendError = TableDoesNotExist
                  | TableAlreadyExists
                  deriving Show

data ExecResult = Created
                | Inserted RowId
                | Selected [Row]
                deriving Show

class Backend b where
    createTable :: b -> TableName -> TableSchema -> Either BackendError b

    lookupTable :: b -> TableName -> Maybe Table

    modifyTable :: b -> TableName -> (Table -> (Table, a)) -> Either BackendError (b, a)

    deleteTable :: b -> TableName -> Either BackendError b

exec :: Backend b => b -> Statement -> Either BackendError (b, ExecResult)
exec b (CreateTable name schema) = (, Created) <$> createTable b name schema
exec b (InsertInto name pairs) = do
    -- FIXME(Sergei): reorder columns to match table schema!
    (b', rowId) <- insertInto b name $ Row { rowId = Nothing
                                           , rowData = map snd pairs
                                           }
    return $ (b', Inserted rowId)
exec b (SelectAll name) = do
    rows <- selectAll b name
    return $ (b, Selected rows)

withTable :: Backend b => b -> TableName -> (Table -> a) -> Maybe a
withTable b name f = f <$> lookupTable b name

insertInto :: Backend b => b -> TableName -> Row -> Either BackendError (b, RowId)
insertInto b name row@(Row { rowId = Nothing, .. }) =
    modifyTable b name $ \t@(Table { .. }) ->
        let t' = t { tabData = row { rowId = Just tabCounter } : tabData
                   , tabCounter = tabCounter + 1
                   }
        in (t', tabCounter)
insertInto _b _name _row = error "insertInto: existing Row"

selectAll :: Backend b => b -> TableName -> Either BackendError [Row]
selectAll b name =
    maybe (Left TableDoesNotExist) Right result
  where
    result = withTable b name tabData
