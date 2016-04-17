--Data: nieuwe algebraisch datatype aanpaken, bvb boolean of de maybe monad.
--Type: alias van algebraisch datatype aanmaken.
--Newtype: ook alias aanmaken maar syntax zoals bij data mogelijk, bestaat niet
--         meer at runtime.
import Control.Monad
import Control.Applicative

newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

parse :: Parser a -> String -> a
parse m s = one [ x  | (x, t) <- apply m s, t == ""] where
  one []                  = error "no parse"
  one [x]                 = x
  one xs | length xs > 1  = error "ambigious parse"

instance Functor Parser where
  fmap f m = Parser (\s -> [ (f x, t) | (x, t) <- apply m s] )

instance Applicative Parser where
  pure a  = Parser (\s -> [(a, s)])
  m <*> f = undefined

instance Monad Parser where
  return = pure
  m >>= k  = Parser (\s -> [ (b, t)  | (a, c) <- apply m s,
                                       (b, t) <- apply (k a) c ])

instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

instance MonadPlus Parser where
  mzero     = Parser (\s -> []) --Lambda kan ook geschreven worden als const [], const: a -> b -> a
  mplus a b = Parser (\s -> apply a s ++ apply b s)

char :: Parser Char
char = Parser f where
  f []    = []
  f (c:s) = [(c,s)]

spot :: (Char -> Bool) -> Parser Char
spot p = Parser (\s ->
   do { c <- char; guard (p c); return c })

spot' :: (Char -> Bool) -> Parser Char
spot' p = Parser(\s -> char >>= \c -> guard(p c) >> return c)
