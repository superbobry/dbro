{-# LANGUAGE NamedFieldPuns #-}

module Data.Bro.Backend.Util
  ( rowSize
  ) where

import Data.Int (Int64)
import qualified Data.ByteString.Char8 as S
import Foreign.Storable (sizeOf)

import Data.Bro.Types (Row(..), ColumnType(..), ColumnName, ColumnValue(..), TableSchema, Expr(..), Projection(..))

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
        word +  -- row id
        word    -- list lengthf52ce92

neg :: ColumnValue -> ColumnValue
neg (VarcharValue _) = error "neg"
neg (IntegerValue i) = IntegerValue (-i)
neg (DoubleValue d) = DoubleValue (-d)

add :: ColumnValue -> ColumnValue -> ColumnValue
add (VarcharValue v1) (VarcharValue v2) = VarcharValue (S.concat [v1, v2])
add _ (VarcharValue _) = error "add"
add (VarcharValue _) _ = error "add"
add (IntegerValue i1) (IntegerValue i2) = IntegerValue (i1 + i2)
add (IntegerValue i) (DoubleValue d) = DoubleValue ((fromIntegral i) + d)
add (DoubleValue d) (IntegerValue i) = DoubleValue (d + (fromIntegral i))
add (DoubleValue d1) (DoubleValue d2) = DoubleValue (d1 + d2)

sub :: ColumnValue -> ColumnValue -> ColumnValue
sub (VarcharValue _) (VarcharValue _) = error "sub"
sub _ (VarcharValue _) = error "sub"
sub (VarcharValue _) _ = error "sub"
sub (IntegerValue i1) (IntegerValue i2) = IntegerValue (i1 - i2)
sub (IntegerValue i) (DoubleValue d) = DoubleValue ((fromIntegral i) - d)
sub (DoubleValue d) (IntegerValue i) = DoubleValue (d - (fromIntegral i))
sub (DoubleValue d1) (DoubleValue d2) = DoubleValue (d1 - d2)

mul :: ColumnValue -> ColumnValue -> ColumnValue
mul (VarcharValue _) (VarcharValue _) = error "mul"
mul _ (VarcharValue _) = error "mul"
mul (VarcharValue _) _ = error "mul"
mul (IntegerValue i1) (IntegerValue i2) = IntegerValue (i1 * i2)
mul (IntegerValue i) (DoubleValue d) = DoubleValue ((fromIntegral i) * d)
mul (DoubleValue d) (IntegerValue i) = DoubleValue (d * (fromIntegral i))
mul (DoubleValue d1) (DoubleValue d2) = DoubleValue (d1 * d2)

divide :: ColumnValue -> ColumnValue -> ColumnValue
divide (VarcharValue _) (VarcharValue _) = error "divide"
divide _ (VarcharValue _) = error "div"
divide (VarcharValue _) _ = error "div"
divide (IntegerValue i1) (IntegerValue i2) = IntegerValue (div i1 i2)
divide (IntegerValue i) (DoubleValue d) = DoubleValue ((fromIntegral i) / d)
divide (DoubleValue d) (IntegerValue i) = DoubleValue (d / (fromIntegral i))
divide (DoubleValue d1) (DoubleValue d2) = DoubleValue (d1 / d2)

reduce :: Expr -> Expr
reduce (Negate (Const c)) = Const (neg c)
reduce (Add (Const c1) (Const c2)) = Const (add c1 c2)
reduce (Sub (Const c1) (Const c2)) = Const (sub c1 c2)
reduce (Multiply (Const c1) (Const c2)) = Const (mul c1 c2)
reduce (Divide (Const c1) (Const c2)) = Const (divide c1 c2)
reduce (Negate expr) = Negate (reduce expr)
reduce (Add expr1 expr2) = Add (reduce expr1) (reduce expr2)
reduce (Sub expr1 expr2) = Sub (reduce expr1) (reduce expr2)
reduce (Multiply expr1 expr2) = Multiply (reduce expr1) (reduce expr2)
reduce (Divide expr1 expr2) = Divide (reduce expr1) (reduce expr2)
reduce expr = expr

substitute :: [(ColumnName, ColumnValue)] -> Expr -> Expr
substitute [] expr = expr
substitute _ (Const c) = Const c
substitute cnv (Field columnName) = case lookup columnName cnv of 
    Just columnValue -> Const columnValue
    Nothing          -> error "susbtitute"
substitute cnv (Negate expr) = Negate (substitute cnv expr)
substitute cnv (Add expr1 expr2) = Add (substitute cnv expr1) (substitute cnv expr2)
substitute cnv (Sub expr1 expr2) = Sub (substitute cnv expr1) (substitute cnv expr2)
substitute cnv (Multiply expr1 expr2) = Multiply (substitute cnv expr1) (substitute cnv expr2)
substitute cnv (Divide expr1 expr2) = Divide (substitute cnv expr1) (substitute cnv expr2)

project :: TableSchema -> Row -> Projection -> Row
project tableSchema row projection = row