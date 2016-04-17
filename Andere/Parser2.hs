import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

--Apply a parser
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) s = f s

--Return a parsed value, assuming at least one succesful parse
parse :: Parser a -> String -> a
parse m s = one [x | (x,t) <- apply m s, t==""]
   where
     one [] = error "No parse"
     one [x] = x
     one xs | length xs > 1 = error "Ambiguous parse"

instance Functor Parser where
  fmap f p = Parser (\s -> [(f x, s) | (x, s) <- apply p s])

instance Applicative Parser where
  pure x  = Parser (\s -> [(x, s)])
  p <*> f = undefined
  --p <*> f = Parser (\s -> [(y x, s) | (x, t) <- apply p s, (y, u) <- apply f s])

--Parsing is a Monad
instance Monad Parser where
  return = pure
  m >>= k  = Parser (\s -> [(y, u) | (x, t) <- apply m s, (y, u) <- apply (k x) t])

--Parsing is a MonadPlus
instance MonadPlus Parser where
  mzero     = Parser (\s -> [])
  mplus m n = Parser (\s -> apply m s ++ apply n s)

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus
  some = undefined
  many = undefined

--Parse a character
char' :: Parser Char
char' = Parser f
  where
    f [] = []
    f (c:s) = [(c,s)]

-- Parse a character satisfying a predicate (e.g. isDigit)
spot :: (Char -> Bool) -> Parser Char
spot p = do
  c <- char'
  guard (p c)
  return c

-- Match a given character
token :: Char -> Parser Char
token c = spot (== c)

--Match a given String
match :: String -> Parser String
match [] = return []
match (x:xs) = do
  y <- token x
  ys <- match xs
  return (y:ys)

--Match a sequence
--Zero or more occurences
star :: Parser a -> Parser [a]
star p = plus p `mplus` return []

--One or more occurences
plus :: Parser a -> Parser [a]
plus p = do
  x <- p
  xs <- star p
  return (x:xs)

--Match a natural number
parseNat :: Parser Int
parseNat = do
  s <- plus (spot isDigit)
  return (read s)

--Match a negative number
parseNeg :: Parser Int
parseNeg = do
  token '-'
  n <- parseNat
  return (-n)

--match an integer
parseInt :: Parser Int
parseInt = parseNat `mplus` parseNeg

--Parsing expressions
data Exp = Lit Int
         | Exp :+: Exp
         | Exp :*: Exp
         deriving (Eq,Show)

evalExp :: Exp -> Int
evalExp (Lit n) = n
evalExp (e :+: f) = evalExp e + evalExp f
evalExp (e :*: f) = evalExp e * evalExp f

parseExp :: Parser Exp
parseExp = parseLit `mplus` parseAdd `mplus` parseMul
  where
    parseLit = do
      n <- parseInt
      return (Lit n)
    parseAdd = do
      token '('
      d <- parseExp
      token '+'
      e <- parseExp
      token ')'
      return (d :+: e)
    parseMul = do
      token '('
      d <- parseExp
      token '*'
      e <- parseExp
      token ')'
      return (d :*: e)
