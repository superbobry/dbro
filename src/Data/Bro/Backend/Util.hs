{-# LANGUAGE NamedFieldPuns #-}

module Data.Bro.Backend.Util
  ( rowSize
  ) where

import Foreign.Storable (sizeOf)

import Data.Bro.Types (ColumnType(..), TableSchema)

rowSize :: TableSchema -> Int
rowSize tabSchema = (+ overhead) $! sum $! do
    (_name, t) <- tabSchema
    return $! 1 + case t of  -- +1 for tag value.
        DoubleColumn    -> 25
        IntegerColumn   -> 4
        VarcharColumn l -> fromIntegral l + (sizeOf (undefined :: Int))
  where
    overhead =
        1 +                            -- maybe tag
        (sizeOf (undefined :: Int)) +  -- row id
        (sizeOf (undefined :: Int))    -- list length