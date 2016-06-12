{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

data Person = P { name :: String,
                  addr :: Address,
                  salary :: Int }
     deriving (Show)

data Address = A { road :: String,
                   city :: String,
                   postcode :: String }
     deriving (Show)

setName :: String -> Person -> Person
setName n p = p { name = n }

setPostcode :: String -> Person -> Person
setPostcode pc p = p { addr = (addr p) { postcode = pc }  }

setAddr :: Address -> Person -> Person
setAddr a p = p { addr = a }

data LensR s a = L { viewR :: s -> a,
                     setR  :: a -> s -> s }

viewl :: LensR s a -> s -> a
viewl = viewR

setl :: LensR s a -> a -> s -> s
setl = setR

compose :: LensR s1 s2 -> LensR s2 a2 -> LensR s1 a2
compose l1 l2 = L {
    viewR = viewR l2 . viewR l1,
    setR = \a s -> setR l1 (setR l2 a (viewR l1 s)) s
}

-- lname :: LensR Person String
laddr :: LensR Person Address
laddr = L {viewR = addr, setR = setAddr}

lsalary :: LensR Person Int
lsalary = L {viewR = salary, setR = \s p -> p { salary = s } }

lpostcode :: LensR Address String
lpostcode = L {viewR = postcode, setR = \pc a -> a { postcode = pc } }

person = P { name = "Ward",
               addr = A { road = "Kuiperstraat",
                          city = "Dikkele",
                          postcode = "9630" },
               salary = 42 }

newperson = setl (laddr `compose` lpostcode) "9860" person

type Lens s a = forall f . Functor f => (a -> f a) -> s -> f s

lensToLensR :: Lens  s a -> LensR s a
lensToLensR = undefined

lensRToLens :: LensR s a -> Lens  s a
lensRToLens = undefined

view :: Lens s a -> s -> a
view lns = getConst . lns Const

set :: Lens s a -> a -> s -> s
set = undefined

newtype Const v a = Const v

--Phantom type
getConst :: Const v a -> v
getConst (Const x) = x

instance Functor (Const a) where
  fmap _ (Const x) = Const x


main :: IO ()
main = print newperson
