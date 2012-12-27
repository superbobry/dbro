module Data.Bro.Condition
  ( evalCondition,
    evalRange
  ) where

import Data.Bro.Expr (evalExpr)
import Data.Bro.Types   (Expr(..), Condition(..), ColumnName, ColumnValue(..),
                        TableIndex, IndexName, Range, RangeValue(..), ElemRange)

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
evalRange [] _cond = []
evalRange index (Just condition) = [ (snd (head index), f (fst (head index)) condition) ]
  where
    f :: ColumnName -> Condition -> Range
    f name cond = case cond of
        Equals      cn (Const c) | cn == name -> [(cv2rv c, cv2rv c)]
        NotEquals   cn (Const c) | cn == name -> [(MinusInf, cv2rv (c - 1)), (cv2rv(c + 1), PlusInf)]
        GreaterThan cn (Const c) | cn == name -> [(cv2rv(c + 1), PlusInf)]
        LowerThan   cn (Const c) | cn == name -> [(MinusInf, cv2rv(c-1))]
        Or          cond1    cond2     -> rangeOr (f name cond1) (f name cond2)
        And         cond1    cond2     -> rangeAnd (f name cond1) (f name cond2)
        _any                           -> [(MinusInf, PlusInf)]

evalRange index _cond = [ (snd (head index), [(MinusInf, PlusInf)]) ]
rangeOr :: Range -> Range -> Range
rangeOr r1 r2 = concat $ map elemOr [(l, r) | l <- r1, r <- r2]

elemOr :: (ElemRange, ElemRange) -> [ElemRange]
elemOr (r1@(v1l, v1r), r2@(v2l, v2r)) = if v1l < v2l
                                          then if v1r > v2l
                                            then [(v1l, v2r)]
                                            else [r1, r2]
                                          else if v2r > v1l
                                            then [(v2l, v1r)]
                                            else [r1, r2]

rangeAnd :: Range -> Range -> Range
rangeAnd r1 r2 = map elemAnd [(l, r) | l <- r1, r <- r2]

elemAnd :: (ElemRange, ElemRange) -> ElemRange
elemAnd ((v1l, v1r), (v2l, v2r)) = if nl <= nr then (nl, nr) else (MinusInf, MinusInf)
                                  where
                                    nl = max v1l v2l
                                    nr = min v1r v2r

cv2rv :: ColumnValue -> RangeValue
cv2rv (IntegerValue i) = NumericRange i
cv2rv _any = error "cv2rv"
