{-# LANGUAGE GADTs #-}

module Data.Bro.Expr
  ( evalExpr
  ) where

import Safe (readDef)
import qualified Data.ByteString.Char8 as S

import Data.Bro.Types (Expr(..), ColumnName, ColumnValue(..))

instance Num ColumnValue where
    (+) = cv_binOp (+)
    (*) = cv_binOp (*)
    (-) = cv_binOp (-)

    negate = cv_unOp negate
    abs = cv_unOp abs
    signum = cv_unOp signum
    fromInteger = IntegerValue . fromIntegral

instance Fractional ColumnValue where
    (/) = cv_binOp (/)
    fromRational = undefined

evalExpr :: Expr -> [(ColumnName, ColumnValue)] -> ColumnValue
evalExpr (Const v) _ctx = v
evalExpr (Field n) ctx = case lookup n ctx of
    Just v  -> v
    Nothing -> error "evalExpr"
evalExpr (Negate e) ctx = negate (evalExpr e ctx)
evalExpr (Add e1 e2) ctx = (evalExpr e1 ctx) + (evalExpr e2 ctx)
evalExpr (Sub e1 e2) ctx = (evalExpr e1 ctx) - (evalExpr e2 ctx)
evalExpr (Multiply e1 e2) ctx = (evalExpr e1 ctx) * (evalExpr e2 ctx)
evalExpr (Divide e1 e2) ctx = (evalExpr e1 ctx) / (evalExpr e2 ctx)

cv_unOp :: (Num a, Read a, a ~ Double) => (a -> a) -> ColumnValue -> ColumnValue
cv_unOp unOp v = cv_cast v (unOp $ cv_toDouble v)

cv_binOp :: (Num a, Read a, a ~ Double)
         => (a -> a -> a) -> ColumnValue -> ColumnValue -> ColumnValue
cv_binOp binOp v1 v2 =
    -- All calculations are done in 'Double', the type of the resulting
    -- term is determined from the first argument.
    cv_cast v1 (cv_toDouble v1 `binOp` cv_toDouble v2)

cv_toDouble :: ColumnValue -> Double
cv_toDouble (IntegerValue i) = fromIntegral i
cv_toDouble (DoubleValue d) = d
cv_toDouble (VarcharValue s) = readDef 0 $! S.unpack s

cv_cast :: ColumnValue -> Double -> ColumnValue
cv_cast (IntegerValue _i) = IntegerValue . round
cv_cast (DoubleValue _i)  = DoubleValue
cv_cast (VarcharValue _s) = VarcharValue . S.pack . show
