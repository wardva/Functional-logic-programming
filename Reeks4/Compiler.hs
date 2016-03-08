import StackMachine
import Evaluator

compile :: Exp -> Prog
compile (Const a) = [IPush a]
compile (Add a b) = compile a ++ compile b ++ [IAdd]
compile (Sub a b) = compile a ++ compile b ++ [ISub]
compile (Mul a b) = compile a ++ compile b ++ [IMul]
