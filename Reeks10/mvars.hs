import System.IO
import Control.Concurrent.STM
import Control.Concurrent


newtype STMMVAR a = STMMVAR (TVar (Maybe a))

newEmptySTMMVAR :: STM (STMMVAR a)
newEmptySTMMVAR = undefined 

takeSTMMVAR :: STMMVAR a -> STM a
takeSTMMVAR (STMMVAR t) = undefined

putSTMMVAR :: STMMVAR a -> a -> STM ()
putSTMMVAR (STMMVAR t) a = undefined


write ta tb = do threadDelay 1000000
                 atomically $ do putSTMMVAR ta "Hello"
                                 putSTMMVAR tb "World"
                                 return ()

read ta tb = atomically $ do  
                            a <- takeSTMMVAR ta
                            b <- takeSTMMVAR tb
                            return (a,b)

main =  do  ta <- atomically newEmptySTMMVAR
            tb <- atomically newEmptySTMMVAR
            forkIO $ write ta tb
            x <- read ta tb 
            putStrLn $ show x
            return ()
