import Data.Text
import Control.Monad
import qualified Data.Map as Map

main :: IO ()
main = void userLogin

--Login
data LoginError = InvalidEmail | NoSuchUser | WrongPassword deriving Show

getDomain :: String -> Either LoginError String
getDomain email =
  case splitOn' "@" email of
    [_, domain] -> Right domain
    _              -> Left InvalidEmail

splitOn' :: String -> String -> [String]
splitOn' s d = let (s', d') = (pack s, pack d) in
  fmap unpack (splitOn s' d')

--Naief
printResult' :: Either LoginError String -> IO ()
printResult' domain =
  case domain of
    Right text        -> putStrLn ("Domain: " ++ text)
    Left InvalidEmail -> putStrLn "ERROR: Invalid domain"

--Beter
either' :: (LoginError -> String) -> (String -> String) -> (Either LoginError String -> String)
either' f' f m = case m of
  Right x  -> f  x
  Left e   -> f' e

printResult :: Either LoginError String -> IO ()
printResult = putStrLn . either' (const "Error: Invalid domain")
                                 (++ "Domain: ")

--Side effects
getToken :: IO (Either LoginError String)
getToken = do
  putStrLn "Enter email address"
  email <- getLine
  return (getDomain email)

--Authentication
users :: Map.Map String String
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

--Login: LELIJK
userLogin :: IO (Either LoginError String)
userLogin = do
  token <- getToken
  case token of
    Right domain ->
      case Map.lookup domain users of
        Just userpw -> do
          putStrLn "Enter password:"
          password <- getLine
          if userpw == password
            then return token
            else return (Left WrongPassword)
        Nothing -> return (Left NoSuchUser)
    left -> return left

--Eigen Monad om dit te vereenvoudigen
data EitherIO e a = EitherIO { runEitherIO :: IO (Either e a) }

instance Functor (EitherIO e) where
  fmap f ex = wrapped
    where unwrapped   = runEitherIO ex
          fmapped     = fmap (fmap f) unwrapped
          wrapped     = EitherIO fmapped

instance Applicative (EitherIO e) where
  pure    = EitherIO . return . Right
  f <*> x = EitherIO $ fmap (<*>) (runEitherIO f) <*> runEitherIO x
