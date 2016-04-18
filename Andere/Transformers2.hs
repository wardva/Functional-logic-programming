import Data.Text
import Control.Monad
import Control.Applicative
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

--Authentication
users :: Map.Map String String
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

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

getToken :: EitherIO LoginError String
getToken = do
  EitherIO $ fmap Right (putStrLn "Enter email address:")
  input <- EitherIO $ fmap Right getLine
  EitherIO . return $ getDomain input

--Helpfuncties: lifting
liftEither :: Either e a -> EitherIO e a
liftEither = EitherIO . return

liftIO :: IO a -> EitherIO e a
liftIO = EitherIO . fmap Right

getToken' :: EitherIO LoginError String
getToken' = do
  liftIO $ putStrLn "Enter email address"
  input <- liftIO getLine
  liftEither $ getDomain input

--Login: MOOI
userLogin :: EitherIO LoginError String
userLogin = do
  token     <- getToken
  --The maybe function takes a default value, a function, and a Maybe value. If the Maybe value is Nothing,
  --the function returns the default value. Otherwise, it applies the function to the value inside the Just and returns the result.
  userpw    <- maybe (liftEither (Left NoSuchUser)) return (Map.lookup token users)
  password  <- liftIO (putStrLn "Enter your password: " >> getLine)
  if userpw == password
    then return token
    else liftEither (Left WrongPassword)

--General
data ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance Functor m => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT

instance Applicative m => Applicative (ExceptT e m) where
  pure    = ExceptT . pure . Right
  f <*> x = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT x)

instance Monad m => Monad (ExceptT e m) where
  return  = ExceptT . return . Right
  x >>= f = ExceptT $ runExceptT x >>= either (return . Left) (runExceptT . f)
