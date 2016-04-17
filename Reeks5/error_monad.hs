import Prelude hiding (Either, Left, Right)
data Either a b = Left a | Right b deriving Show

myDiv :: Int -> Int -> Either String Int
myDiv x y
    | y == 0 = Left "can\'t divide by zero"
    | x `mod` y /= 0 = Left "not exact"
    | otherwise = Right (x `div` y)

instance Functor (Either a) where
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right (f b)

instance Applicative (Either a) where
    pure             =  Right
    Left a <*> _ = Left a
    Right f <*> r = fmap f r

instance Monad (Either a) where
  return = Right
  (Left a) >>= _ = Left a
  (Right b) >>= k = k b

calculate :: Int -> Either String Int
calculate x = myDiv x 2 >>= myDiv 100 >>= myDiv 1000

calculate' :: Int -> Either String Int
calculate' x = do
  let a = 5
  b <- myDiv 10 a
  myDiv x b

reverse' :: Maybe String -> Maybe String
reverse' x = do
  waarde <- x
  let resultaat = reverse waarde
  return resultaat

reverse'' x = x >>= reverse
