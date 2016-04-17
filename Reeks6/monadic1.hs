import Prelude hiding (lookup)

type Name  = String

data E a  = Success a | Error String

data Term  = Var Name
           | Con Int
           | Add Term Term
           | Lam Name Term
           | App Term Term

data Value = E Value
           | Num Int
           | Fun (Value -> E Value)

type Environment = [(Name,Value)]

-- Test data

term0 = App (Lam "x" (Add (Var "x") (Var "x")))
             (Add (Con 10) (Con 11))

term1 = App (Con 1) (Con 2)

--Variation One
unitE                   = Success
errorE                  = Error
(Success a) `bindE` k   = k a
(Error s) `bindE` k     = Error s
showE (Success a)       = "Success: " ++ showval a
showE (Error s)         = "Error: " ++ s


showval              :: Value -> String
showval (Num i)      = show i
showval (Fun f)      = "<function>"

interp               :: Term -> Environment -> E Value
interp (Var x) e     = lookup x e
interp (Con i) e     = unitE (Num i)
interp (Add u v) e   = interp u e `bindE` (\a ->
                       interp v e `bindE` (\b ->
                       add a b))
interp (Lam x v) e   = unitE (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e   = interp t e `bindE` (\f ->
                       interp u e `bindE` (\a ->
                       apply f a))

lookup               :: Name ->Environment -> E Value
lookup x []          = errorE "Waarde niet gevonden"
lookup x ((y,b):e)   = if x==y then unitE b else lookup x e

add                  :: Value -> Value -> E Value
add (Num i)  (Num j) = unitE (Num (i+j))
add a b              = errorE "Kan enkel 2 getallen optellen"

apply                :: Value -> Value -> E Value
apply (Fun k) a      = k a
apply f a            = errorE "Apply kan enkel op Fun worden uitgevoerd"

test                 :: Term -> String
test t               = showE (interp t [])
