module Data.Bro.Backend.Result
  ( BackendResult(..)
  ) where

import Data.Bro.Types (Row, RowId)

data BackendResult = Created
                   | Inserted RowId
                   | Selected [Row]
                   | Updated Int
                   | Deleted Int
				   | CreatedIndex
