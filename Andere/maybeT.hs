import Control.Monad
import Data.Char
import Control.Applicative

-- Code from https://en.wikibooks.org/wiki/Haskell/Monad_transformers
-- Adjusted to work with ghc 7.10 by Christophe Scholliers

-- The validation test could be anything we want it to be
-- but we picked this one to be being particularly annoying
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Functor (MaybeT m) where
     --Moet teruggeven: MaybeT m b
     fmap f m = MaybeT (fmap (\x -> (fmap f x)) (runMaybeT m))

instance (Monad m) => Applicative (MaybeT m) where
     pure  x =  MaybeT (pure (Just x))
     f <*> x =  MaybeT $ fmap (<*>) (runMaybeT f) <*> runMaybeT x

instance Monad m => Monad (MaybeT m) where
    return  = pure
    -- The signature of (>>=), specialized to MaybeT m
    x >>= f = MaybeT $ do
                maybeValue <- runMaybeT x
                case maybeValue of
                  Nothing -> return Nothing
                  Just x -> runMaybeT $ f x

    --(runMaybeT x) : (m (Maybe a))
    --f: a -> MaybeT b

instance Monad m => Alternative (MaybeT m) where
    (<|>) = mplus
    empty = mzero

instance Monad m => MonadPlus (MaybeT m) where
    mzero     = MaybeT $ return Nothing
    mplus x y = MaybeT $ runMaybeT x >> runMaybeT y


liftIO x = MaybeT (fmap Just x)

getValidPassphrase :: MaybeT IO String
getValidPassphrase = do s <- liftIO getLine
                        guard (isValid s) -- MonadPlus provides guard.
                        return s

askPassphrase :: MaybeT IO ()
askPassphrase = do liftIO $ putStrLn "Insert your new passphrase:"
                   value <- getValidPassphrase
                   liftIO $ putStrLn "Storing in database..."

askPassword :: MaybeT IO ()
askPassword = do liftIO $ putStrLn "Insert your new password:"
                 value <- msum $ repeat getValidPassphrase
                 liftIO $ putStrLn "Storing in database..."

main :: IO(Maybe ())
main = runMaybeT askPassword
