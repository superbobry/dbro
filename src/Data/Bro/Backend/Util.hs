{-# LANGUAGE NamedFieldPuns #-}

module Data.Bro.Backend.Util
  ( rowSize
  ) where

import Data.Int (Int64)

import Foreign.Storable (sizeOf)

import Data.Bro.Types (ColumnType(..), TableSchema)

rowSize :: TableSchema -> Int
rowSize tabSchema = (+ overhead) $! sum $! do
    -- FIXME(Sergei): WHY 'Data.Binary' uses 8-byte integers on
    -- on 32-bit platforms?
    (_name, t) <- tabSchema
    return $! 1 + case t of  -- +1 for tag value.
        DoubleColumn    -> 25
        IntegerColumn   -> 4
        VarcharColumn l -> fromIntegral l + word
  where
    word :: Int
    word = sizeOf (undefined :: Int64)

    overhead =
        1    +  -- data segment tag
        1    +  -- row id maybe tag
        1    +  -- isDeleted flag
        word +  -- row id
        word    -- list lengthf52ce92
