import Control.Monad.STM
import Control.Concurrent.STM

newtype STMMVAR a = STMMVAR (TVar [a])

newEmptySTMMVAR :: STM (STMMVAR a)
newEmptySTMMVAR = fmap STMMVAR (newTVar [])

takeSTMMVAR :: STMMVAR a -> STM a
takeSTMMVAR (STMMVAR t) = do
  m <- readTVar t
  case m of
    (x:xs) -> writeTVar t xs >> return x
    [] -> retry

putSTMMVAR :: STMMVAR a -> a -> STM ()
putSTMMVAR (STMMVAR t) x = readTVar t >>= \xs -> writeTVar t (xs ++ [x])

main :: IO ()
main = undefined
