{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Data.Bro.Backend.Memory
  ( MemoryBackend
  ) where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Bro.Backend (Backend(..), BackendError(..))
import Data.Bro.Types (TableName, Table(..))


data MemoryBackend = MemoryBackend { memTables :: Map TableName Table }

instance Backend MemoryBackend where
    lookupTable (MemoryBackend { .. }) name = M.lookup name memTables

    createTable b@(MemoryBackend { .. }) name schema =
        case lookupTable b name of
            Just _table -> Left TableAlreadyExists
            Nothing     ->
                return b { memTables = M.insert name table memTables }
      where
        table :: Table
        table = Table { tabName = name
                      , tabSchema = schema
                      , tabData = []
                      , tabCounter = 0
                      }

    modifyTable b@(MemoryBackend { .. }) name f =
        case lookupTable b name of
            Just table ->
                let (acc, table') = f table
                    b' = b { memTables = M.insert name table' memTables }
                in return (acc, b')
            Nothing    -> Left TableDoesNotExist

    deleteTable b@(MemoryBackend { .. }) name =
        case lookupTable b name of
            Just _table -> return b { memTables = M.delete name memTables }
            Nothing     -> Left TableDoesNotExist
