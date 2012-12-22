{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Data.Bro.Backend.Memory
  ( MemoryBackend
  , makeMemoryBackend
  ) where

import Data.List (find)
import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.Error (throwError)
import Control.Monad.State (gets)
import Control.Monad.Trans (lift)
import Data.Default (def)
import qualified Data.Conduit.List as CL

import Data.Bro.Backend.Class (Backend(..), Query(..),
                               withTable, fetchTable, transformRow)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Util (rowSize)
import Data.Bro.Types (TableName, Table(..), Row(..), Projection(..))

data MemoryBackend = MemoryBackend { memTables :: Map TableName Table
                                   , memData   :: Map TableName [Row]
                                   }

instance Backend MemoryBackend where
    lookupTable name = M.lookup name <$> gets memTables

    insertTable name schema = do
        res <- lookupTable name
        case res of
            Just _table -> throwError TableAlreadyExists
            Nothing     -> modifyBackend $ \b@(MemoryBackend { .. }) ->
                b { memTables = M.insert name table memTables }
      where
        table :: Table
        table = def { tabName = name
                    , tabSchema = schema
                    , tabRowSize = rowSize schema
                    }

    modifyTable name f = withTable name $ \table ->
        modifyBackend $ \b@(MemoryBackend { .. }) ->
            b { memTables = M.insert name (f table) memTables }

    deleteTable name = withTable name $ \_table ->
        modifyBackend $ \b@(MemoryBackend { .. }) ->
            b { memTables = M.delete name memTables }

instance Query MemoryBackend where
    selectAll name = do
        rows <- lift $ do
            void $ fetchTable name
            M.findWithDefault [] name <$> gets memData
        CL.sourceList rows

    insertInto name row@(Row { rowId = Nothing, .. }) = do
        tabCounter <- withTable name (return . tabCounter)
        modifyBackend $ \b@(MemoryBackend { .. }) ->
            let rows = M.findWithDefault [] name memData
                row' = row { rowId = Just tabCounter }
            in b { memData = M.insert name (row':rows) memData }
        modifyTable name $ \table@(Table { tabSize }) ->
            table { tabCounter = tabCounter + 1, tabSize = tabSize + 1 }
        return $! tabCounter
    insertInto _name _row = error "Inserting existing Row"

    update name exprs c = do
        rows <- select name (Projection []) c
        table <- fetchTable name
        modifyBackend $ \b@(MemoryBackend { .. }) ->
            let rows' = map (transformRow table exprs) rows
                oldrows = M.findWithDefault [] name memData
            in b { memData = M.insert name (change oldrows rows') memData }
        return $ length rows
      where
        change (h:oldList) newList = nh:(change oldList) newList
          where
            nh = case find (\node -> (rowId node) == rowId h) newList of
                Nothing -> h
                Just node -> node
        change [] _ = []

makeMemoryBackend :: MemoryBackend
makeMemoryBackend = MemoryBackend { memTables = M.empty, memData = M.empty }
