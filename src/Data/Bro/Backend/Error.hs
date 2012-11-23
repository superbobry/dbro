module Data.Bro.Backend.Error
  ( BackendError(..)
  ) where

import Control.Monad.Error (Error(..))

data BackendError = TableDoesNotExist
                  | TableAlreadyExists
                  | UnknownError String
    deriving Show

instance Error BackendError where
    strMsg = UnknownError
