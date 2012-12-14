{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections #-}

module Data.Bro.Backend.Memory.Tests
  ( tests
  ) where

import Control.Applicative ((<$>))

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Data.Bro.Backend.Class (Backend(..), insertInto, selectAll)
import Data.Bro.Backend.Memory (MemoryBackend, makeMemoryBackend)
import Data.Bro.Monad (runBro)
import Data.Bro.Types (Row(..), ColumnType(..), ColumnValue(..))


data Backend b => WithTable b = WithTable b deriving Show


tests :: Test
tests = testGroup "Data.Bro.Backend.Memory.Tests"
    [ testCase "insert -> select" test_insertSelect
    ]

test_insertSelect :: Assertion
test_insertSelect = do
    Right (rowId', rows) <- flip runBro makeMemoryBackend $! do
        insertTable "yada" [("yada", IntegerColumn)]
        rowId' <- insertInto "yada" $ Row Nothing [IntegerValue 1]
        (rowId', ) <$> selectAll "yada"

    -- Note(Sergei): just a naive insert-select case.
    length rows @=? 1
    rows @=? [Row { rowId = Just rowId', rowData = [IntegerValue 1]}]
