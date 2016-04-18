import Prelude hiding (lookup)
type Name  = String
type Position = Int
data Term  = Var Name
           | Con Int
           | Add Term Term
           | Lam Name Term
           | App Term Term
           | At Position Term

data Value = Num Int
           | Fun (Value -> E Value)

type Environment = [(Name,Value)]

-- Test data

term2 :: Term
term2 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Con 10) (Con 11))

-- Variation Two => Positions
pos0 :: Position
pos0 = 0

data  E a               = Success a | Error String
type P a                = Position -> E a

unitP                  :: a -> P a
unitP a                 = \p -> unitE a
errorP s                = \p -> errorE (showP p ++ ": " ++ s)

bindP                  :: P a -> (a -> P b) -> P b
m `bindP` k             = \p -> m p `bindE` (\x -> k x p)

showP                  :: (Position -> E Value) -> String
showP m                 = showE (m pos0)

resetP                 :: Position -> P a -> P a
resetP q m              = \p -> m q

-- Variation One => Error messages

unitE                   :: a -> E a
unitE                   = Success

errorE                  :: String -> E a
errorE                  = Error

bindE                   :: E a -> (a -> E b) -> E b
(Success a) `bindE` k   = k a
(Error s)   `bindE` _   = Error s

showE                   :: E Value -> String
showE (Success a)       = "Success: " ++ showval a
showE (Error s)         = "Error: " ++ s

showval                 :: Value -> String
showval (Num i)         = show i
showval (Fun _)         = "<function>"

interp               :: Term -> Environment -> P Value
interp (Var x) e     = lookup x e
interp (Con i) _     = unitP (Num i)
interp (Add u v) e   = interp u e `bindP` (\a ->
                       interp v e `bindP` (\b ->
                       add a b))
interp (Lam x v) e   = unitP (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e   = interp t e `bindP` (\f ->
                       interp u e `bindP` (\a ->
                       apply f a))


interp (At p t) e    = resetP p (interp t e)
--interp (At p t) e    = resetP p (\_ -> interp t e) p


lookup               :: Name ->Environment -> E Value
lookup x []          = errorE $ "Unbound variable: " ++ x
lookup x ((y,b):e)   = if x==y then unitE b else lookup x e

add                  :: Value -> Value -> E Value
add (Num i) (Num j)  = unitE (Num (i+j))
add a b              = errorE $ "Error while adding. Should be numbers: "
                          ++ showval a ++ ", " ++ showval b ++ "."

apply                :: Value -> Value -> E Value
apply (Fun k) a      = k a
apply f _            = errorE $ "Error while applying a function. Should be a function: "
                          ++ showval f ++ "."

test                 :: Term -> String
test t               = showE (interp t [])

main :: IO ()
main = putStrLn $ test term2
