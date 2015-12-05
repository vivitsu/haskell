module Expr where
import ExprT

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  mul = Mul
  add = Add
