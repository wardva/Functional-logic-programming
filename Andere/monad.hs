data Sheep = Sheep { name::String,
                     mother::Maybe Sheep,
                     father::Maybe Sheep }

maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = case mother s of
  Nothing -> Nothing
  Just m -> father m

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = case mother s of
  Nothing -> Nothing
  Just m -> case father m of
    Nothing -> Nothing
    Just gf -> father gf

-- comb is a combinator for sequencing operations that return Maybe
comb :: Maybe a -> (a -> Maybe b) -> Maybe b
comb Nothing _ = Nothing
comb (Just x) f = f x

maternalGrandfather' :: Sheep -> Maybe Sheep
maternalGrandfather' s = Just s `comb` mother `comb` father

mothersPaternalGrandfather' :: Sheep -> Maybe Sheep
mothersPaternalGrandfather' s = Just s `comb` mother `comb` father `comb` father

class Chain m where
  comb' :: m a -> (a -> m b) -> m b
  wrap :: a -> m a

instance Chain Maybe where
  comb' Nothing _ = Nothing
  comb' (Just x) f = f x
  wrap = Just

mothersPaternalGrandfather'' :: Sheep -> Maybe Sheep
mothersPaternalGrandfather'' s = wrap s `comb'` mother `comb'` father `comb'` father

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = return s >>= father >>= mother >>= mother

fathersMaternalGrandmother' :: Sheep -> Maybe Sheep
fathersMaternalGrandmother' s = do
  f <- father s
  gm <- mother f
  mother gm
-- ~~
mothersPaternalGrandfather''' :: Sheep -> Maybe Sheep
mothersPaternalGrandfather''' s = father s >>= (\f ->
   mother f >>= (\gm ->
   mother gm))
