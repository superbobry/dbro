{-# LANGUAGE NamedFieldPuns #-}

module Data.Bro.Backend.Util
  ( rowSize,
    rangeToVal
  ) where

import Data.Int (Int64, Int32)
import Foreign.Storable (sizeOf)

import Data.Bro.Types (ColumnType(..), TableSchema, RangeValue(..))

rowSize :: TableSchema -> Int32
rowSize tabSchema = (+ overhead) $! sum $! do
    -- FIXME(Sergei): WHY 'Data.Binary' uses 8-byte integers on
    -- on 32-bit platforms?
    (_name, t) <- tabSchema
    return $! 1 + case t of  -- +1 for tag value.
        DoubleColumn    -> 25
        IntegerColumn   -> 4
        VarcharColumn l -> fromIntegral l + word
  where
    word :: Int32
    word = fromIntegral $ sizeOf (undefined :: Int64)

    overhead =
        1    +  -- data segment tag
        1    +  -- row id maybe tag
        1    +  -- deleted flag
        word +  -- row id
        word    -- list length

rangeToVal :: RangeValue -> Int32
rangeToVal (NumericRange n) = n
rangeToVal MinusInf = minBound::Int32
rangeToVal PlusInf = maxBound::Int32

--minInt :: Int
--minInt = minBound::Int

--maxInt :: Int
--maxInt = maxBound::Int
