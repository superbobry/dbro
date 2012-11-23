{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Data.Bro.Backend
  ( Result(..)
  , exec
  ) where

import Control.Applicative ((<$>))

import Data.Bro.Backend.Class (Backend, Query(..))
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Result (Result(..))
import Data.Bro.Monad (Bro)
import Data.Bro.Types (Row(..), Statement(..))
import qualified Data.Bro.Backend.Class as Backend

exec :: (Query b, Backend b) => Statement -> Bro BackendError b Result
exec s = case s of
     CreateTable name schema -> Result <$> Backend.insertTable name schema
     SelectAll name -> Result <$> Backend.selectAll name
     InsertInto name pairs ->
         let row = Row { rowId = Nothing, rowData = map snd pairs } in
         Result <$> Backend.insertInto name row