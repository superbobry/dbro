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

import Data.Bro.Backend (Backend(..), BackendError(..))
import Data.Bro.Types (TableName, Table(..))

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

    modifyTable name f = do
        res <- lookupTable name
        case res of
            Just table -> modify $ \b@(MemoryBackend { .. }) ->
                b { memTables = M.insert name (f table) memTables }
            Nothing    -> throwError TableDoesNotExist

    deleteTable name = do
        res <- lookupTable name
        case res of
            Just _table -> modify $ \b@(MemoryBackend { .. }) ->
                b { memTables = M.delete name memTables }
            Nothing     -> throwError TableDoesNotExist

makeMemoryBackend :: MemoryBackend
makeMemoryBackend = MemoryBackend { memTables = M.empty }
