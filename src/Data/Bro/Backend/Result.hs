module Data.Bro.Backend.Result
  ( BackendResult(..)
  ) where

import Data.Conduit (Source)

import Data.Bro.Backend.Class (Backend, Query)
import Data.Bro.Backend.Error (BackendError)
import Data.Bro.Monad (Bro)
import Data.Bro.Types (Row, RowId)

data (Backend b, Query b) => BackendResult b
    = Created
    | Inserted RowId
    | Selected (Source (Bro BackendError b) Row)
    | Updated Int
    | Deleted Int
    | CreatedIndex
