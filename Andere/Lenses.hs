data Person = P { name:: String,
                  addr::Address,
                  salary::Int }

data Address = A { road::String,
                   city :: String,
                   postcode :: String }

Addr    :: LensR Person Address
City    :: LensR Address String

lname   :: LensR Person String
laddr   :: LensR Person Address
lsalary :: LensR Person Int

data LensR s a = L { viewR :: s -> a ,
                     setR :: a -> s -> s }

compose :: LensR s1 s2 -> LensR s2 a2 -> LensR s1 a2
compose l1 l2 = L {
    viewR = ?,
    setR = ?
}

main :: IO ()
main = undefined
