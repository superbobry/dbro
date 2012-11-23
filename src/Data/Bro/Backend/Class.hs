{-# LANGUAGE OverloadedStrings #-}

module Data.Bro.Backend.Class
  ( Backend(..)
  , Query(..)
  , withTable
  ) where

import qualified Data.ByteString.Char8 as S

import Control.Monad.Error (throwError)

import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Monad (Bro)
import Data.Bro.Types (TableName, TableSchema, Table, Row, RowId)

class Backend b where
    insertTable :: TableName -> TableSchema -> Bro BackendError b ()

    lookupTable :: TableName -> Bro BackendError b (Maybe Table)

    modifyTable :: TableName -> (Table -> Table) -> Bro BackendError b ()
    modifyTable = undefined

    deleteTable :: TableName -> Bro BackendError b ()

class Backend b => Query b where
    selectAll :: TableName -> Bro BackendError b [Row]

    insertInto :: TableName -> Row -> Bro BackendError b RowId


withTable :: Backend b
          => TableName
          -> (Table -> Bro BackendError b a)
          -> Bro BackendError b a
withTable name f = do
    res <- lookupTable name
    maybe (throwError TableDoesNotExist) f res