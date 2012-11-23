{-# LANGUAGE OverloadedStrings, ExistentialQuantification, FlexibleInstances #-}

module Data.Bro.Backend.Result
  ( BackendResult(..)
  , Result(..)
  ) where

import qualified Data.ByteString.Char8 as S

import Data.Bro.Types (Row, RowId)
import Data.Bro.Backend.Error (BackendError)

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
    -- FIXME(Sergei): use CSV formatting here.
    format = S.pack . show

instance BackendResult BackendError where
    format = S.pack . show