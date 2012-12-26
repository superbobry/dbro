module Data.Bro.Condition
  ( evalCondition,
    evalRange
  ) where

import Data.Bro.Expr (evalExpr)
import Data.Bro.Types   (Expr(..), Condition(..), ColumnName, ColumnValue(..), 
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

rangeOr :: Range -> Range -> Range
rangeOr range1 range2 = range1 ++ range2 -- Note(Pavel): Need some optimisatios here.

rangeAnd :: Range -> Range -> Range
rangeAnd range1 range2 = range1 ++ range2 -- Note(Pavel): Not good. Need to be fixed.

cv2rv :: ColumnValue -> RangeValue
cv2rv (IntegerValue i) = NumericRange i
cv2rv _any = error "cv2rv"

evalRange :: TableIndex -> Maybe Condition -> [(IndexName, Range)]
evalRange [] _cond = []
evalRange index (Just condition) = [ (snd (head index), f (fst (head index)) condition) ]
  where
    f :: ColumnName -> Condition -> Range
    f name cond = case cond of
        Equals      condName (Const c) | condName == name -> [(cv2rv c,cv2rv c)]
        NotEquals   condName (Const c) | condName == name -> [(MinusInf, cv2rv (c-1)), (cv2rv(c+1), PlusInf)]
        GreaterThan condName (Const c) | condName == name -> [(cv2rv(c+1), PlusInf)]
        LowerThan   condName (Const c) | condName == name -> [(MinusInf, cv2rv(c-1))]
        Or          cond1    cond2     -> rangeOr (f name cond1) (f name cond2)
        And         cond1    cond2     -> rangeAnd (f name cond1) (f name cond2)
        _any                           -> [(MinusInf, PlusInf)]
evalRange index _cond = [ (snd (head index), [(MinusInf, PlusInf)]) ]