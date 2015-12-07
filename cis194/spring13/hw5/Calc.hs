module Calc where
import Expr
import ExprT as E
import Parser
import StackVM as VM

eval :: ExprT -> Integer
eval (Lit n) = n
eval (E.Add e1 e2) = eval e1 + eval e2
eval (E.Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit E.Add E.Mul s of
        Just a -> Just (eval a)
        Nothing -> Nothing

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

compile :: String -> Maybe Program
compile = parseExp lit add mul
