{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, CPP #-}

#ifndef DEBUG
#error "Please run 'cabal configure --enable-tests -fdebug'"
#endif

module Data.Bro.Parser.Tests
  ( tests
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Word (Word8)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as S

import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Property, Gen,
                        oneof, listOf1, elements, printTestCase)

import Data.Bro.Parser (statement, projection, condition, expr)
import Data.Bro.Simple (simplify)
import Data.Bro.Types (Row(..), ColumnType(..), ColumnValue(..), ColumnName,
                       TableSchema, Projection(..), Expr(..),
                       Condition(..), Statement(..))

-- | Generate a valid SQL symbol name, currently a stub, which generates
-- words in the alphabet /[a-fA-F0-9]/.
symbol :: Gen S.ByteString
symbol = S.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'])

instance Arbitrary S.ByteString where
    arbitrary = symbol  -- restrict 'ByteString' to ASCII subset.

instance Arbitrary Row where
    arbitrary = Row Nothing <$> listOf1 arbitrary

instance Arbitrary Projection where
    arbitrary = simplify . Projection <$> listOf1 arbitrary

instance Arbitrary Expr where
    arbitrary = simplify <$> (compound =<< arbitrary)
      where
        basic :: Gen Expr
        basic = oneof [ Const <$> arbitrary
                      , Field <$> arbitrary
                      ]

        compound :: Word8 -> Gen Expr
        compound 0 = basic
        compound n = oneof [ Add <$> next <*> next
                           , Negate <$> next
                           , Sub <$> next <*> next
                           , Multiply <$> next <*> next
                           , Divide <$> next <*> next
                           ]
            where next = oneof [basic, compound (n `div` 2)]

instance Arbitrary Condition where
    arbitrary = simplify <$> (compound =<< arbitrary) where
      basic :: Gen Condition
      basic = oneof [ Equals <$> arbitrary <*> arbitrary
                    , NotEquals <$> arbitrary <*> arbitrary
                    , GreaterThan <$> arbitrary <*> arbitrary
                    , LowerThan <$> arbitrary <*> arbitrary
                    ]

      compound :: Word8 -> Gen Condition
      compound _ = basic
      compound n = oneof [ Or <$> next <*> next
                         , And <$> next <*> next
                         ]
        where next = oneof [basic, compound (n `div` 2)]

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
    arbitrary = simplify <$>
                oneof [ CreateTable <$> arbitrary <*> listOf1 arbitrary
                      , InsertInto <$> arbitrary <*> listOf1 arbitrary
                      , Select <$> arbitrary <*> arbitrary <*> arbitrary
                      , Update <$> arbitrary <*> listOf1 arbitrary <*> arbitrary
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

instance ToSQL Expr where
    toSQL (Const v) = toSQL v
    toSQL (Field n) = n
    toSQL (Negate e)  = S.concat ["-", "(", toSQL e, ")"]
    toSQL (Add e1 e2) = S.concat ["(", toSQL e1, " + ", toSQL e2, ")"]
    toSQL (Sub e1 e2) = S.concat ["(", toSQL e1, " - ", toSQL e2, ")"]
    toSQL (Multiply e1 e2) = S.concat [toSQL e1, " * ", toSQL e2]
    toSQL (Divide e1 e2) = S.concat [toSQL e1, " / ", toSQL e2]

instance ToSQL Projection where
    toSQL (Projection []) = "*"
    toSQL (Projection exprs) = S.intercalate ", " $ map toSQL exprs

instance ToSQL Condition where
    toSQL (Equals name e) = S.concat [name, " = ", toSQL e]
    toSQL (NotEquals name e) = S.concat [name, " != ", toSQL e]
    toSQL (GreaterThan name e) = S.concat [name, " > ", toSQL e]
    toSQL (LowerThan name e) = S.concat [name, " < ", toSQL e]
    toSQL (And c1 c2) = S.concat [toSQL c1, " && ", toSQL c2]
    toSQL (Or c1 c2) = S.concat [toSQL c1, " || ", toSQL c2]

instance ToSQL [(ColumnName, Expr)] where
    toSQL [] = error "impossible"
    toSQL bs = S.intercalate ", " $! do
        (name, expr) <- bs
        return $! S.concat [name, " = ", toSQL expr]

instance ToSQL Statement where
    toSQL (CreateTable table schema) =
        S.pack $! printf "CREATE TABLE %s(%s);"
        (S.unpack table)
        (S.unpack $! toSQL schema)
    toSQL (Select table p Nothing) =
        S.pack $! printf "SELECT %s FROM %s;"
        (S.unpack $! toSQL p)
        (S.unpack table)
    toSQL (Select table p (Just c)) =
        S.pack $! printf "SELECT %s FROM %s WHERE %s;"
        (S.unpack $! toSQL p)
        (S.unpack table)
        (S.unpack $! toSQL c)
    toSQL (Update table bindings Nothing) =
        S.pack $! printf "UPDATE %s SET %s;"
        (S.unpack table)
        (S.unpack $! toSQL bindings)
    toSQL (Update table bindings (Just c)) =
        S.pack $! printf "UPDATE %s SET %s WHERE %s;"
        (S.unpack table)
        (S.unpack $! toSQL bindings)
        (S.unpack $! toSQL c)
    toSQL (Delete table Nothing) =
        S.pack $! printf "DELETE FROM %s;" (S.unpack table)
    toSQL (Delete table (Just c)) =
        S.pack $! printf "DELETE FROM %s WHERE %s;"
        (S.unpack table)
        (S.unpack $! toSQL c)
    toSQL (InsertInto table pairs) =
        let (names, values) = unzip pairs in S.pack $!
        printf "INSERT INTO %s(%s) VALUES (%s);"
        (S.unpack table)
        (S.unpack $! S.intercalate ", " names)
        (S.unpack  . S.intercalate ", " $ map toSQL values)

tests :: Test
tests = testGroup "Data.Bro.Parser.Tests"
    [ testProperty "statement" (genericParseUnparse statement)
    , testProperty "condition" (genericParseUnparse condition)
    , testProperty "projection" (genericParseUnparse projection)
    , testProperty "expr" (genericParseUnparse expr)
    ]

prop_statementParseUnparse :: Statement -> Property
prop_statementParseUnparse = genericParseUnparse statement

genericParseUnparse :: (Eq a, Show a, ToSQL a, Arbitrary a)
                    => Parser a -> a -> Property
genericParseUnparse p s =
    printTestCase ("Generated SQL: " ++ S.unpack sql) $
    printTestCase ("Generated AST: " ++ show s) $
    printTestCase ("Parsed       : " ++ show result) $
    case result of
        Left _msg -> False
        Right s'  -> s == s'
  where
    sql = toSQL s
    result = parseOnly p sql
