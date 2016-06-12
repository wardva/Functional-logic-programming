{-# LANGUAGE RankNTypes #-}

-- Identity Functor
newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- Peel of the constructor
getId  :: Identity s -> s
getId (Identity x) = x

-- Const functor ignores f and just returns a value
newtype Const v b = Const v
instance Functor (Const b)  where
  fmap f (Const x) = Const x

-- Note that Const is a phantom type here because b is not used
-- it is basically ignored but satisfies the type checker
getConst :: Const v b -> v
getConst (Const x) = x

data LensR s a = L  { viewR :: s -> a,
                      setR :: a -> s -> s }

type Lens s a = forall f . Functor f => (a -> f a) -> s -> f s


-- Given a lens structure s and a value a gives you a function that
-- takes a structure and returns an a.
view :: Lens s a -> s -> a
view lns = getConst . lns Const

-- Helper function
liftId new_value _ = Identity new_value

set  :: Lens s a -> a -> s -> s
set lns new_value structure = getId (lns (liftId new_value) structure)

lensToLensR :: Lens s a -> LensR s a
lensToLensR ln = L { viewR = view ln,
                     setR = set ln }

lensRtoLens :: LensR s a -> Lens s a
lensRtoLens ln transform structure = fmap (\x -> setR ln x structure) (transform (viewR ln structure))


-----------------------------------------------------------------
-- Tuples
-----------------------------------------------------------------
--_1 :: Functor f => (a -> f a) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) a
_1 trans (x,y) = fmap (\z -> (z, y)) (trans x)

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) a
_2 trans (x,y) = fmap (\z -> (x, z)) (trans y)


--------------------------------------------------------------
-- Lens s1 a             :: Functor f => (a -> f a) -> s1 -> f s1
-- Lens s2 a             :: Functor f => (a -> f a) -> s2 -> f s2
-- Lens (Either s1 s2) a :: Functor f => (a -> f a) -> (Either s1 s2) -> f (Either s1 s2)
--------------------------------------------------------------
choosing :: Lens s1 a -> Lens s2 a -> Lens (Either s1 s2) a
choosing l1 l2 trans s = case s of
  Left  a -> fmap Left (l1 trans a)
  Right b -> fmap Right (l2 trans b)


data Book  = Book  { booktitle :: String, author   :: String }
data Movie = Movie { movietitle :: String, producer :: String }

book  = Book "Hitchhiker's Guide to the Galaxy" "Douglas Adams"
movie = Movie "Fight Club"  "David Fincher"

getbtitle :: Lens Book String
getbtitle = \trans (Book x y) -> fmap (\z -> Book z y) (trans x)

getmtitle :: Lens Movie String
getmtitle = \trans (Movie x y) -> fmap (\z -> Movie z y) (trans x)

test1 = view (choosing getbtitle getmtitle) $ Right movie
test2 = view (choosing getbtitle getmtitle) $ Left book

main :: IO ()
main = undefined
