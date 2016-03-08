module Evaluator (Exp(Const, Add, Sub, Mul), eval) where

data Exp = Const Int | Add Exp Exp | Sub Exp Exp| Mul Exp Exp
           deriving Show

eval :: Exp -> Int
eval (Const a) = a
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
