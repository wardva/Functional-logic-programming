type Name = String

--Term = uitdrukking
data Term = Var Name |
            Con Integer |
            Add Term Term |
            Lam Name Term |
            App Term Term
            deriving (Show, Eq)


--Value = resultaat van uitdrukking
data Value = Wrong |
             Num Integer |
             Fun (Value->Value)

instance Show Value where
  show Wrong    = "Wrong"
  show (Num i)  = "Int " ++ show i
  show (Fun _)  = "Function"

type Env = [(Name,Value)]

getVar :: Env -> Name -> Value
getVar [] = Wrong
getVar ((k',v):r) k
    | k == k' = v
    | otherwise getVar r k

--Apply values
add :: Value -> Value -> Value
add (Num x) (Num y) = Num (x+y)
add _ _             = Wrong

apply :: Value -> Value -> Value
apply (Fun f) t2 = f t2
apply _ _        = Wrong

--Eval
eval :: Term -> Env -> Value
eval (Var x) env      = getVar env x
eval (Con x) _        = Num x
eval (Add t1 t2) env  = add (eval t1 env) (eval t2 env)
eval (Lam n b) env    = Fun (\x -> eval b ((n,x) : env))
eval (App t1 t2) env  = apply (eval t1 env) (eval t2 env)
