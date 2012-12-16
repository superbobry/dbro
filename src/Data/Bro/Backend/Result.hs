module Data.Bro.Backend.Result
  ( BackendResult(..)
  ) where

import Data.Bro.Types (Table, Row, RowId)

data BackendResult = Created
                   | Inserted RowId
                   | Selected Table [Row]
                   | Updated Int
