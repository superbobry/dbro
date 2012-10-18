{-# LANGUAGE OverloadedStrings #-}

module Data.Bro.Parser.Tests
  ( tests
  ) where

import Control.Applicative ((<$>), (<*>), pure)

import Data.Attoparsec.Text (parseOnly)
import Data.Text.Format (format)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Property,
                        oneof, printTestCase)
import Test.QuickCheck.Instances ()
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.Bro.Parser (statement)
import Data.Bro.Types (ColumnType(..), ColumnValue(..),
                       TableSchema, Statement(..))

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
    arbitrary = oneof [ CreateTable <$> arbitrary <*> arbitrary
                      , InsertInto <$> arbitrary <*> arbitrary
                      , SelectAll <$> arbitrary
                      ]

tests :: Test
tests = testGroup "Data.Bro.Parser.Tests"
    [ testProperty "statementParseUnparse" prop_statementParseUnparse
    ]

prop_statementParseUnparse :: Statement -> Property
prop_statementParseUnparse s =
    let sql = LT.toStrict $ unparse s in
    printTestCase (T.unpack sql) $
    parseOnly statement sql == Right s
  where
    unparse :: Statement -> LT.Text
    unparse (CreateTable table schema) =
        format "CREATE TABLE {}({});"
        [LT.fromStrict table, unparseSchema schema]
    unparse (InsertInto table cvs) =
        let (names, values) = unzip cvs in
        format "INSERT INTO {}({}) VALUES ({});"
        [ LT.fromStrict table
        , LT.intercalate ", " $ map LT.fromStrict names
        , LT.intercalate ", " $ map unparseColumnValue values
        ]
    unparse (SelectAll table) =
        format "SELECT * FROM {};" $ [LT.fromStrict table]

    unparseSchema :: TableSchema -> LT.Text
    unparseSchema schema = LT.intercalate ", " $ do
        (name, t) <- schema
        return $!
            format "{} {}" [LT.fromStrict name, unparseColumnType t]

    unparseColumnType :: ColumnType -> LT.Text
    unparseColumnType IntegerColumn = "INT"
    unparseColumnType DoubleColumn = "DOUBLE"
    unparseColumnType (VarcharColumn l) = format "VARCHAR({})" $ show l

    unparseColumnValue :: ColumnValue -> LT.Text
    unparseColumnValue (IntegerValue x) = LT.pack $ show x
    unparseColumnValue (DoubleValue d) = LT.pack $ show d
    unparseColumnValue (VarcharValue t) = LT.fromStrict t
