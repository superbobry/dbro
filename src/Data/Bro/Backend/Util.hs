{-# LANGUAGE NamedFieldPuns #-}

module Data.Bro.Backend.Util
  ( rowSize
  , rangeToVal
  , wrap
  , unwrap
  ) where

import Data.Int (Int64, Int32)
import Foreign.Storable (sizeOf)
import qualified Data.ByteString.Char8 as S

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

-- | @wrap n s@ padds row chunk with a prefix of @\0\0...\xff@,
-- where @\xff@ starts data segment. A given chunk is expected to
-- be shorter than @n - 1@ to fit the @\xff@ tag.
wrap :: Int -> S.ByteString -> S.ByteString
wrap n s =
    let len = fromIntegral $ S.length s
        pad = fromIntegral $ n - 1 - len
    in case compare len (n - 1) of
        EQ -> S.cons '\xff' s
        LT -> S.append (S.replicate pad '\0') $ S.cons '\xff' s
        GT -> error "row chunk size overflow"

-- | @unwrap s@ removes padding, added by @wrap@.
unwrap :: S.ByteString -> S.ByteString
unwrap = S.tail . S.dropWhile (/= '\xff')
