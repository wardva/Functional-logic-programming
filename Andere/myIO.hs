module MyIO(MyIO, putChar', getChar', convert') where
  import Data.Char
  type Input      = String
  type Remainder  = String
  type Output     = String

  data MyIO a     = MyIO (Input -> (a, Remainder, Output))

  apply :: MyIO a -> Input -> (a, Remainder, Output)
  apply (MyIO f) inp = f inp

  putChar' :: Char -> MyIO ()
  putChar' ch = MyIO (\input -> ((), input, [ch]))

  getChar' :: MyIO Char
  getChar' = MyIO (\(c:rest) -> (c, rest, []))

  putStr' :: String -> MyIO ()
  putStr' = foldr (>>) (return ()) . map putChar'

  getLine' :: MyIO String
  getLine' = do
    x <- getChar'
    if x == '\n'
      then return []
      else getLine' >>= \xs -> return (x:xs)

  echo' :: MyIO String
  echo' = do
    line <- getLine'
    if line == ""
      then return ""
      else return $ map toUpper line

--Interact neemt een String -> String functie als argument en voert
--de functie uit met de volledige input buffer als parameter. Vervolgens
--wordt een IO operatie uitgevoerd met als output het resultaat van de
--uitgevoerde functie. (String -> String) -> IO ()
  convert' :: MyIO () -> IO ()
  convert' m = interact (\input ->
               let (_, _, out) = apply m input
               in out)

  instance Functor MyIO where
    fmap f m = MyIO (\input -> let (x, remainder, output) = apply m input
                          in (f x, remainder, output))

  instance Applicative MyIO where
    pure x = MyIO (\input -> (x, input, []))
    m <*> f = undefined

  instance Monad MyIO where
    return = pure
    m >>= k  = MyIO (\input ->
                let (x, rem1, out1) = apply m input in
                let (y, rem2, out2) = apply (k x) rem1 in
                    (y, rem2, out1 ++ out2))

{-
  Console: "ABCDEF"
  getChar' >>= putChar'
  input = "ABCDEF"
  apply m input = ('A', "BCDEF", [])
  k x ~ putChar 'A'
  apply (k x) = ((), "BCDEF", "A")
  ======================================
  apply (getChar >>= \x -> getChar >>= \y -> return [x,y]) "abc"
  ("ab", "c", "")
  ======================================
  apply (putChar ’A’ >> putChar ’B’) "def"
  putChar 'A' = \input -> ("", input, "A")
  putChar 'B' = \input -> ("", input, "B")
  putChar ’A’ >> putChar ’B’ =
     \input -> ("", input, "AB")
  result = ("", "def", "AB")
  =======================================
  apply (getChar >>= \x -> putChar (toUpper x)) "abc"
  getChar = \input@(c:cs) -> (c, cs, "")
  getChar >>= \x -> putChar (toUpper x) = \input@(c:cs) -> ((), cs, "C")
  apply (getChar >>= \x -> putChar (toUpper x)) "abc" =
      ((), "bc", "A")
-}
