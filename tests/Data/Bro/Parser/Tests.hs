{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Data.Bro.Parser.Tests
  ( tests
  ) where

import Control.Applicative ((<$>), (<*>), pure)

import Data.Attoparsec.Text (parseOnly)
import Data.Text.Format (format)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Property, Gen,
                        oneof, listOf1, elements, printTestCase)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.Bro.Parser (statement)
import Data.Bro.Types (ColumnType(..), ColumnValue(..),
                       TableSchema, Statement(..))

-- | Generate a valid SQL symbol name, currently a stub, which generates
-- words in the alphabet /[a-fA-F0-9]/.
symbol :: Gen T.Text
symbol = T.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'])

instance Arbitrary T.Text where
    arbitrary = symbol  -- restrict 'Text' to ASCII subset.

instance Arbitrary ColumnType where
    arbitrary = oneof [ pure IntegerColumn
                      , pure DoubleColumn
                      , VarcharColumn <$> arbitrary
                      ]

instance Arbitrary ColumnValue where
    arbitrary = oneof [ IntegerValue <$> arbitrary
                      , DoubleValue <$> arbitrary
                      , VarcharValue <$> arbitrary
                      ]

instance Arbitrary Statement where
    arbitrary = oneof [ CreateTable <$> arbitrary <*> listOf1 arbitrary
                      , InsertInto <$> arbitrary <*> listOf1 arbitrary
                      , SelectAll <$> arbitrary
                      ]

class ToSQL a where
    toSQL :: a -> LT.Text

instance ToSQL ColumnType where
    toSQL IntegerColumn = "INT"
    toSQL DoubleColumn = "DOUBLE"
    toSQL (VarcharColumn l) = format "VARCHAR({})" [show l]

instance ToSQL ColumnValue where
    toSQL (IntegerValue x) = LT.pack $ show x
    toSQL (DoubleValue d) = LT.pack $ show d
    toSQL (VarcharValue t) = LT.pack $ show t

instance ToSQL TableSchema where
    toSQL schema = LT.intercalate ", " $ do
        (name, t) <- schema
        return $! format "{} {}" [LT.fromStrict name, toSQL t]

instance ToSQL Statement where
    toSQL (CreateTable table schema) =
        format "CREATE TABLE {}({});" [LT.fromStrict table, toSQL schema]
    toSQL (InsertInto table cvs) =
        let (names, values) = unzip cvs in
        format "INSERT INTO {}({}) VALUES ({});"
        [ LT.fromStrict table
        , LT.intercalate ", " $ map LT.fromStrict names
        , LT.intercalate ", " $ map toSQL values
        ]
    toSQL (SelectAll table) = format "SELECT * FROM {};" [LT.fromStrict table]

tests :: Test
tests = testGroup "Data.Bro.Parser.Tests"
    [ testProperty "statementParseUnparse" prop_statementParseUnparse
    ]

prop_statementParseUnparse :: Statement -> Property
prop_statementParseUnparse s =
    printTestCase ("SQL: " ++ T.unpack sql) $
    printTestCase ("AST: " ++ show s) $
    printTestCase ("RES: " ++ show result) $
    case result of
        Left _msg -> False
        Right s'  -> s == s'
  where
    sql = LT.toStrict $ toSQL s
    result = parseOnly statement sql
