{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Data.Bro.Backend.Memory
  ( MemoryBackend
  , makeMemoryBackend
  ) where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.List (find)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Control.Monad.Error (throwError)
import Control.Monad.State (gets)
import Control.Monad.Trans (lift)
import Data.Conduit (($$))
import Data.Default (def)
import qualified Data.Conduit.List as CL

import Data.Bro.Backend.Class (Backend(..), Query(..),
                               withTable, fetchTable, updateRow)
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
        rows <- select name (Projection []) c $$ CL.consume
        table <- fetchTable name
        modifyBackend $ \b@(MemoryBackend { .. }) ->
            let rows' = map (updateRow table exprs) rows
                oldrows = M.findWithDefault [] name memData
            in b { memData = M.insert name (change oldrows rows') memData }
        return $ length rows
      where
        -- FIXME(Sergei): ugly! please avoid double 'where', if possible.
        change (h:oldList) newList = nh:(change oldList) newList
          where
            nh = fromMaybe h $ find (\node -> (rowId node) == rowId h) newList
        change [] _ = []

makeMemoryBackend :: MemoryBackend
makeMemoryBackend = MemoryBackend { memTables = M.empty, memData = M.empty }
