data Boom a = Leeg | Knoop a (Boom a) (Boom a)

insert :: Ord a => a -> Boom a -> Boom a
insert v Leeg = Knoop v Leeg Leeg
insert v b@(Knoop a l r)
    | v < a     = Knoop a (insert v l) r
    | v > a     = Knoop a l (insert v r)
    | otherwise = b

search :: Ord a => a -> Boom a -> Bool
search _ Leeg = False
search v (Knoop a l r)
    | v == a = True
    | v < a = search v l
    | v > a = search v r

instance Show a => Show (Boom a) where
  show (Knoop a l r) = "Waarde: " ++ show a ++ ", l: "  ++ show l ++ ", r: " ++ show r
  show Leeg = "Lege boom"

instance Eq a => Eq (Boom a) where
  Leeg == Leeg = True
  Knoop a l1 r1 == Knoop b l2 r2 = (a == b) && (l1 == l2) && (r1 == r2)
  _ == _ = False

instance Functor Boom where
  fmap _ Leeg = Leeg
  fmap f (Knoop a l r) = Knoop (f a) (fmap f l) (fmap f r)
