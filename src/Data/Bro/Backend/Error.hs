module Data.Bro.Backend.Error
  ( BackendError(..)
  ) where

import Control.Monad.Error (Error(..))

import Data.Bro.Types (ColumnName, ColumnType, ColumnValue)

data BackendError = TableDoesNotExist
                  | TableAlreadyExists
                  | IndexAlreadyExists
                  | ColumnDoesNotExist
                  | ColumnTypeMismatch ColumnValue ColumnType
                  | ColumnTypeUnsupported ColumnType
                  | ColumnValueMissing ColumnName
                  | UnknownError String
    deriving Show

instance Error BackendError where
    strMsg = UnknownError
