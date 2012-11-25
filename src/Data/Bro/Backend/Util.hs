{-# LANGUAGE NamedFieldPuns #-}

module Data.Bro.Backend.Util
  ( rowSize
  ) where

import Data.Bro.Types (ColumnType(..), TableSchema)

rowSize :: TableSchema -> Int
rowSize tabSchema = (+ overhead) $! sum $! do
    (_name, t) <- tabSchema
    return $! 1 + case t of  -- +1 for tag value.
        DoubleColumn    -> 25
        IntegerColumn   -> 4
        VarcharColumn l -> fromIntegral l + 8
  where
    overhead = 1 + 8 + 8     -- maybe tag + row id + list length.