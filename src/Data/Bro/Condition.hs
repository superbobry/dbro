module Data.Bro.Condition
  ( evalCondition,
    evalRange
  ) where

import Data.Bro.Expr (evalExpr)
import Data.Bro.Types   (Expr, Condition(..), ColumnName, ColumnValue, 
                        TableIndex, IndexName, Range, RangeValue(..),)

evalCondition :: [(ColumnName, ColumnValue)] -> Condition -> Bool
evalCondition ctx c = case c of
    Equals name e -> f name e (==)
    NotEquals name e -> f name e (/=)
    GreaterThan name e -> f name e (>)
    LowerThan name e -> f name e (<)
    Or e1 e2 -> evalCondition ctx e1 || evalCondition ctx e2
    And e1 e2 -> evalCondition ctx e1 && evalCondition ctx e2
  where
    f :: ColumnName -> Expr -> (ColumnValue -> ColumnValue -> Bool) -> Bool
    f name e op = case lookup name ctx of
        Just v -> v `op` evalExpr ctx e
        Nothing -> False  -- Note(Sergei): no error here!

evalRange :: TableIndex -> Maybe Condition -> [(IndexName, Range)]
evalRange index _cond = []--[ (fst . head index, [(MinusInf, PlusInf)]) ]
