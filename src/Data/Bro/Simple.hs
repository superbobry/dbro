module Data.Bro.Simple
  ( simplify
  ) where

import Control.Applicative ((<$>))

import Data.Bro.Expr ()
import Data.Bro.Types (Expr(..), Projection(..), Condition(..), Statement(..))

class Simple a where
    simplify :: a -> a

instance Simple Expr where
    simplify (Negate (Const c)) = Const (-c)
    simplify (Add (Const c1) (Const c2)) = Const (c1 + c2)
    simplify (Sub (Const c1) (Const c2)) = Const (c1 - c2)
    simplify (Multiply (Const c1) (Const c2)) = Const (c1 * c2)
    simplify (Divide (Const c1) (Const c2)) = Const (c1 * c2)
    simplify (Negate (Negate e)) = simplify e
    simplify (Multiply (Negate e1) e2) = Negate (Multiply (simplify e1) (simplify e2))
    simplify (Multiply e2 (Negate e1)) = Negate (Multiply (simplify e1) (simplify e2))
    simplify (Add e1 e2) = Add (simplify e1) (simplify e2)
    simplify (Sub e1 e2) = Sub (simplify e1) (simplify e2)
    simplify (Multiply e1 e2) = Multiply (simplify e1) (simplify e2)
    simplify (Divide e1 e2) = Divide (simplify e1) (simplify e2)
    simplify e = e

instance Simple Projection where
    simplify (Projection es) = Projection $ map simplify es

instance Simple Condition where
    simplify (Equals n e) = Equals n (simplify e)
    simplify (NotEquals n e) = NotEquals n (simplify e)
    simplify (GreaterThan n e) = GreaterThan n (simplify e)
    simplify (LowerThan n e) = LowerThan n (simplify e)
    simplify (Or e1 e2) = Or (simplify e1) (simplify e2)
    simplify (And e1 e2) = And (simplify e1) (simplify e2)

instance Simple Statement where
    simplify (Select n p c) = Select n (simplify p) (simplify <$> c)
    simplify (Update n bs c) =
        Update n [(name, simplify e) | (name, e) <- bs] (simplify <$> c)
    simplify (Delete n c) = Delete n (simplify <$> c)
    simplify s = s
