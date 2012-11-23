{-# LANGUAGE OverloadedStrings #-}

module Data.Bro.Backend.Class
  ( Backend(..)
  , BackendError(..)
  , BackendResult(..)
  ) where

import qualified Data.ByteString.Char8 as S

import Control.Monad.Error (Error(..))

import Data.Bro.Monad (Bro)
import Data.Bro.Types (TableName, TableSchema, Table(..))

data BackendError = TableDoesNotExist
                  | TableAlreadyExists
                  | UnknownError String
    deriving Show

instance Error BackendError where
    strMsg = UnknownError

class BackendResult r where
    format :: r -> S.ByteString

instance BackendResult () where
    format = const "OK"

instance BackendResult BackendError where
    format = S.pack . show

class Backend b where
    insertTable :: TableName -> TableSchema -> Bro BackendError b ()

    lookupTable :: TableName -> Bro BackendError b (Maybe Table)

    modifyTable :: TableName -> (Table -> Table) -> Bro BackendError b ()
    modifyTable = undefined

    deleteTable :: TableName -> Bro BackendError b ()
