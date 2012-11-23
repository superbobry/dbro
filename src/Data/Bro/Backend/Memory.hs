{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Data.Bro.Backend.Memory
  ( MemoryBackend
  , makeMemoryBackend
  ) where

import Control.Applicative ((<$>))
import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad.Error (throwError)
import Control.Monad.State (gets, modify)

import Data.Bro.Backend.Class (Backend(..), Query(..), withTable)
import Data.Bro.Backend.Error (BackendError(..)    )
import Data.Bro.Types (TableName, Table(..), Row(..))

data MemoryBackend = MemoryBackend { memTables :: Map TableName Table }

instance Backend MemoryBackend where
    lookupTable name = M.lookup name <$> gets memTables

    insertTable name schema = do
        res <- lookupTable name
        case res of
            Just _table -> throwError TableAlreadyExists
            Nothing     -> modify $ \b@(MemoryBackend { .. }) ->
                b { memTables = M.insert name table memTables }
      where
        table :: Table
        table = Table { tabName = name
                      , tabSchema = schema
                      , tabData = []
                      , tabCounter = 1
                      }

    modifyTable name f = withTable name $ \table ->
        modify $ \b@(MemoryBackend { .. }) ->
            b { memTables = M.insert name (f table) memTables }

    deleteTable name = withTable name $ \_table ->
        modify $ \b@(MemoryBackend { .. }) ->
            b { memTables = M.delete name memTables }

instance Query MemoryBackend where
    selectAll name = withTable name (return . tabData)

    insertInto name row@(Row { rowId = Nothing, .. }) = do
        modifyTable name $ \table@(Table { .. }) ->
            table { tabData = row { rowId = Just tabCounter } : tabData
                  , tabCounter = tabCounter + 1
                  }
        withTable name (return . tabCounter)
    insertInto _name _row = error "Inserting existing Row"

makeMemoryBackend :: MemoryBackend
makeMemoryBackend = MemoryBackend { memTables = M.empty }
