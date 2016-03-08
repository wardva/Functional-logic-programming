module StackMachine (Inst(IPush, IAdd, ISub, IMul), Prog, Stack, calc) where

data Inst = IPush Int | IAdd | ISub | IMul
            deriving Show

type Prog = [Inst]
type Stack = [Int]

calc :: Prog -> Int
calc p = eval p []

eval :: Prog -> Stack -> Int
eval [] s = head s
eval (p:ps) ss                   | IPush v <- p = eval ps (v:ss)
eval (p:ps) (s1:s2:ss) = case p of IAdd    -> eval ps ((s1 + s2):ss)
                                   ISub    -> eval ps ((s1 - s2):ss)
                                   IMul    -> eval ps ((s1 * s2):ss)
