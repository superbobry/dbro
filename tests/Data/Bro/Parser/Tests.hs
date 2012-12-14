{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Bro.Parser.Tests
  ( tests
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as S

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Property, Gen,
                        oneof, listOf1, elements, printTestCase)

import Data.Bro.Parser (statement)
import Data.Bro.Types (Row(..), ColumnType(..), ColumnValue(..),
                       TableSchema, Statement(..))

-- | Generate a valid SQL symbol name, currently a stub, which generates
-- words in the alphabet /[a-fA-F0-9]/.
symbol :: Gen S.ByteString
symbol = S.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'])

instance Arbitrary S.ByteString where
    arbitrary = symbol  -- restrict 'Text' to ASCII subset.

instance Arbitrary Row where
    arbitrary = Row Nothing <$> listOf1 arbitrary

instance Arbitrary ColumnType where
    arbitrary = oneof [ pure IntegerColumn
                      , pure DoubleColumn
                      , VarcharColumn <$> arbitrary
                      ]

instance Arbitrary ColumnValue where
    arbitrary = oneof [ IntegerValue <$> arbitrary
                      , DoubleValue <$> almostDouble
                      , VarcharValue <$> arbitrary
                      ]
      where
        almostDouble :: Gen Double
        almostDouble = fromIntegral <$>
                       -- HACK(Sergei): we need this because 'number' in
                       -- 'attoparsec' has reduced accuracy.
                       (truncate <$> (arbitrary :: Gen Double) :: Gen Int)

instance Arbitrary Statement where
    arbitrary = oneof [ CreateTable <$> arbitrary <*> listOf1 arbitrary
                      , InsertInto <$> arbitrary <*> listOf1 arbitrary
                      , SelectAll <$> arbitrary
                      ]

class ToSQL a where
    toSQL :: a -> S.ByteString

instance ToSQL ColumnType where
    toSQL IntegerColumn = "int"
    toSQL DoubleColumn = "double"
    toSQL (VarcharColumn l) = S.pack $ printf "varchar(%s)" (show l)

instance ToSQL ColumnValue where
    toSQL (IntegerValue x) = S.pack $ show x
    toSQL (DoubleValue d) = S.pack $ show d
    toSQL (VarcharValue t) = S.pack $ show t

instance ToSQL TableSchema where
    toSQL schema = S.intercalate ", " $ do
        (name, t) <- schema
        return . S.pack $! printf "%s %s" (S.unpack name) (S.unpack $! toSQL t)

instance ToSQL Statement where
    toSQL (CreateTable table schema) =
        S.pack $! printf "CREATE TABLE %s(%s);"
        (S.unpack table)
        (S.unpack $! toSQL schema)
    toSQL (InsertInto table pairs) =
        let (names, values) = unzip pairs in S.pack $!
        printf "INSERT INTO %s(%s) VALUES (%s);"
        (S.unpack table)
        (S.unpack $! S.intercalate ", " names)
        (S.unpack  . S.intercalate ", " $ map toSQL values)
    toSQL (SelectAll table) =
        S.pack . printf "SELECT * FROM %s;" $! S.unpack table

tests :: Test
tests = testGroup "Data.Bro.Parser.Tests"
    [ testProperty "statementParseUnparse" prop_statementParseUnparse
    ]

prop_statementParseUnparse :: Statement -> Property
prop_statementParseUnparse s =
    printTestCase ("SQL: " ++ S.unpack sql) $
    printTestCase ("AST: " ++ show s) $
    printTestCase ("RES: " ++ show result) $
    case result of
        Left _msg -> False
        Right s'  -> s == s'
  where
    sql = toSQL s
    result = parseOnly statement sql
