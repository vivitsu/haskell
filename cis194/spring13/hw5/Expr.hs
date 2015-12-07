{-# LANGUAGE FlexibleInstances #-}

module Expr where
import ExprT as E
import StackVM as VM

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    mul = E.Mul
    add = E.Add

instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr Bool where
    lit x = x > 0
    mul = (&&)
    add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    mul (MinMax x) (MinMax y) = MinMax (min x y)
    add (MinMax x) (MinMax y) = MinMax (max x y)

instance Expr Mod7 where
    lit n = Mod7 (mod n 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)
    add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)

instance Expr Program where
    lit n = [PushI n]
    mul p1 p2 = p1 ++ p2 ++ [VM.Mul]
    add p1 p2 = p1 ++ p2 ++ [VM.Add]
