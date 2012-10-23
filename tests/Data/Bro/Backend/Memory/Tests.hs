{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Data.Bro.Backend.Memory.Tests
  ( tests
  ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Data.Bro.Backend (Backend(..), insertInto, selectAll)
import Data.Bro.Backend.Memory (MemoryBackend, makeMemoryBackend)
import Data.Bro.Types (Row(..), ColumnType(..), ColumnValue(..))


data Backend b => WithTable b = WithTable b deriving Show


tests :: Test
tests = testGroup "Data.Bro.Backend.Memory.Tests"
    [ testCase "insert -> select" test_insertSelect
    ]

test_insertSelect :: Assertion
test_insertSelect = do
    -- Note(Sergei): just a naive insert-select case.
    length rows @=? 1
    rows @=? [Row { rowId = Just rowId', rowData = [IntegerValue 1]}]
  where
    b :: MemoryBackend
    Right b = createTable makeMemoryBackend "yada" [("yada", IntegerColumn)]
    Right (b', rowId') = insertInto b "yada" $ Row Nothing [IntegerValue 1]
    Right rows = selectAll b' "yada"
