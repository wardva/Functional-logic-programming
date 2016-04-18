import Data.Char
import Data.Maybe
import Control.Monad.Trans
import Control.Monad

isValid :: String -> Bool
isValid s = length s >= 8
         && any isAlpha s
         && any isNumber s
         && any isPunctuation s

getPassphrase :: IO (Maybe String)
getPassphrase = do
  s <- getLine
  if isValid s
    then return $ Just s
    else return Nothing

askPassphrase :: IO ()
askPassphrase = do
  putStrLn "Insert your new passphrase:"
  maybeVal <- getPassphrase
  if isJust maybeVal
    then putStrLn "Storing in database."
    else putStrLn "Passphrase is invalid."

--Transformer
newtype MaybeT m a = MaybeT  { runMaybeT :: m (Maybe a) }

instance (Monad m) => Functor (MaybeT m) where
  fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance (Monad m) => Applicative (MaybeT m) where
  pure    = MaybeT . return . Just
  f <*> x = MaybeT $ fmap (<*>) (runMaybeT f) <*> runMaybeT x

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f = MaybeT $ do
    maybeVal <- runMaybeT x
    maybe (return Nothing) (runMaybeT . f) maybeVal

instance Monad m => MonadPlus (MaybeT m) where
  mzero = MaybeT $ return Nothing
  mplus x y = MaybeT $ do maybeVal <- runMaybeT x
                          case maybeVal of
                            Nothing -> runMaybeT y
                            Just _ -> return maybeVal

getPassword :: MaybeT IO String
getPassword = do
  s <- lift getLine
  guard (isValid s)
  return s

askPassphrase' :: MaybeT IO ()
askPassphrase' = do
  lift $ putStrLn "Insert your new passphrase:"
  value <- getPassword
  lift $ putStrLn "Storing in database"
