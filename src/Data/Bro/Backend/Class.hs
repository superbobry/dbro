module Data.Bro.Backend.Class
  ( Backend(..)
  , BackendError(..)
  ) where

import Control.Monad.Error (Error(..))

import Data.Bro.Types (TableName, TableSchema, Table(..), Row(..), RowId,
                       Statement(..))

import Data.Bro.Monad (Bro)


data BackendError = TableDoesNotExist
                  | TableAlreadyExists
                  | UnknownError
    deriving Show

instance Error BackendError where
    noMsg = UnknownError

class Backend b where
    insertTable :: TableName -> TableSchema -> Bro BackendError b ()

    lookupTable :: TableName -> Bro BackendError b (Maybe Table)

    modifyTable :: TableName -> (Table -> Table) -> Bro BackendError b ()
    modifyTable = undefined

    deleteTable :: TableName -> Bro BackendError b ()
