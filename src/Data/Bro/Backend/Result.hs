{-# LANGUAGE OverloadedStrings, ExistentialQuantification, FlexibleInstances #-}

module Data.Bro.Backend.Result
  ( BackendResult(..)
  , Result(..)
  ) where

import Data.Csv (ToField(..), ToRecord(..))
import qualified Data.Csv as Csv
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V

import Data.Bro.Types (Row(..), RowId, ColumnValue(..))
import Data.Bro.Backend.Error (BackendError)

instance ToField ColumnValue where
    toField (IntegerValue i) = toField i
    toField (DoubleValue d) = toField d
    toField (VarcharValue s) = toField s
    {-# INLINE toField #-}

instance ToRecord Row where
    toRecord = toRecord . rowData
    {-# INLINE toRecord #-}

class BackendResult r where
    format :: r -> S.ByteString

data Result = forall r. BackendResult r => Result r

instance BackendResult Result where
    format (Result r) = format r

instance BackendResult () where
    format = const "OK"

instance BackendResult RowId where
    format = S.pack . show

instance BackendResult [Row] where
    -- FIXME(Sergei): switch to 'Data.Vector'?
    -- TODO(Sergei): add column names and types.
    format = S.concat . L.toChunks . Csv.encode . V.fromList

instance BackendResult BackendError where
    format = S.pack . show