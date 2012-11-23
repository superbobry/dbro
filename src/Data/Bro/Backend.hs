{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Data.Bro.Backend
  ( Backend(..)
  , BackendError(..)
  -- , withTable
  -- , insertInto
  -- , selectAll
  , exec
  ) where

import Control.Applicative ((<$))

import Data.Bro.Backend.Class (Backend, BackendError)
import Data.Bro.Monad (Bro)
import Data.Bro.Types (Row(..), RowId, Statement(..))
import qualified Data.Bro.Backend.Class as Backend

exec :: Backend b => Statement -> Bro BackendError b ()
exec (CreateTable name schema) = Backend.insertTable name schema
-- exec b (InsertInto name pairs) = return $! do
--     -- FIXME(Sergei): reorder columns to match table schema!
--     (b', rowId) <- insertInto b name $ Row { rowId = Nothing
--                                            , rowData = map snd pairs
--                                            }
--     return $ (b', Inserted rowId)
-- exec b (SelectAll name) = do
--     rows <- selectAll b name
--     return $ (b, Selected rows)

-- withTable :: Backend b => b -> TableName -> (Table -> a) -> Maybe a
-- withTable b name f = f <$> lookupTable b name

-- insertInto :: Backend b => b -> TableName -> Row -> Either BackendError (b, RowId)
-- insertInto b name row@(Row { rowId = Nothing, .. }) =
--     modifyTable b name $ \t@(Table { .. }) ->
--         let t' = t { tabData = row { rowId = Just tabCounter } : tabData
--                    , tabCounter = tabCounter + 1
--                    }
--         in (t', tabCounter)
-- insertInto _b _name _row = error "insertInto: existing Row"

-- selectAll :: Backend b => b -> TableName -> Either BackendError [Row]
-- selectAll b name =
--     maybe (Left TableDoesNotExist) Right result
--   where
--     result = withTable b name tabData
