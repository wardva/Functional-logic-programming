{-# LANGUAGE RankNTypes #-}

---------------------------------------------------------------
-- Implementation of lenses based on slides and talk of 
-- Lenses: compositional data access and manipulation.
-- by Simon Peyton Jones
---------------------------------------------------------------

---------------------------------------------------------------
-- Identity Functor
---------------------------------------------------------------
newtype Identity a = Identity a

instance Functor Identity where 
	fmap f (Identity x) = Identity (f x)

-- Peel of the constructor
getId  :: Identity s -> s
getId (Identity x) = x

---------------------------------------------------------------
--- Const functor ignores f and just returns a value
---------------------------------------------------------------
newtype Const v b = Const v
instance Functor (Const b)  where
	fmap f (Const x) = Const x

-- Note that Const is a phantom type here because b is not used
-- it is basically ignored but satisfies the type checker
getConst :: Const v b -> v
getConst (Const x) = x

---------------------------------------------------------------
-- First representation of Lenses as a record 
-- This does not scale very well because for each 
-- operation we need to extend the record 
---------------------------------------------------------------
data LensR s a = L  { viewR :: s -> a , setR :: a -> s -> s }	

----------------------------------------------------------------
-- Second representation of lenses, here we define it in terms 
-- of a functor. It turns out that this representation is 
-- equivalent to the data representation.  
-- 
-- A lens takes two types as arguments, namely the type of the
-- structure and the type of the focus
----------------------------------------------------------------
type Lens s a = forall f . Functor f => (a -> f a) -> s -> f s

-- Given a lens structure s and a value a gives you a function that 
-- takes a structure and returns an a.  
view :: Lens s a -> s -> a
view lns  = undefined 

-- Helper function 
liftId new_value old_value = Identity new_value

set  :: (Lens s a) -> a -> s -> s
set lns new_value structure = undefined

-----------------------------------------------------------------
-- Isomorphism between Lens and LensR
--
-----------------------------------------------------------------
lensToLensR :: Lens s a -> LensR s a
lensToLensR ln = undefined 

lensRtoLens :: LensR s a -> Lens s a 
lensRtoLens ln transform structure = undefined


-----------------------------------------------------------------
-- Tuples 
--
-----------------------------------------------------------------
-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) a 
_1 trans (x,y) = undefined 

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) a 
_2 trans (x,y) = undefined


--------------------------------------------------------------
-- Lens s1 a             :: Functor f => a -> f a -> s1 -> f s1
-- Lens s2 a             :: Functor f => a -> f a -> s2 -> f s2
-- Lens (Either s1 s2) a :: Functor f => a -> f a -> (Either s1 s2) -> f (Either s1 s2)
--------------------------------------------------------------
choosing :: Lens s1 a -> Lens s2 a -> Lens (Either s1 s2) a
choosing  = undefined 


data Book  = Book  { booktitle :: String, author   :: String }
data Movie = Movie { movietitle :: String, producer :: String }

book  = Book "Hitchhiker's Guide to the Galaxy" "Douglas Adams"
movie = Movie "Fight Club"  "David Fincher"

getbtitle :: Lens Book String
getbtitle = undefined 
getmtitle :: Lens Movie String 
getmtitle = undefined

test1 = view (choosing getbtitle getmtitle) $ Right movie
test2 = view (choosing getbtitle getmtitle) $ Left book 
