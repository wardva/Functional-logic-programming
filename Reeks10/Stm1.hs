import Control.Monad.STM
import Control.Concurrent.STM

newtype STMMVAR a = STMMVAR (TVar (Maybe a))

newEmptySTMMVAR :: STM (STMMVAR a)
newEmptySTMMVAR = fmap STMMVAR (newTVar Nothing)

takeSTMMVAR :: STMMVAR a -> STM a
takeSTMMVAR (STMMVAR t) = do
  m <- readTVar t
  case m of
    Just a -> return a
    Nothing -> retry

putSTMMVAR :: STMMVAR a -> a -> STM ()
putSTMMVAR (STMMVAR t) a = writeTVar t (Just a) 
