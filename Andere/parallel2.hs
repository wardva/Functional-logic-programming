import Control.Concurrent

threadA :: MVar Int -> MVar Float -> IO ()
threadA valueToSendMVar valueReceiveMVar = do
  putMVar valueToSendMVar 72
  v <- takeMVar valueReceiveMVar
  print v

threadB :: MVar Int -> MVar Float -> IO ()
threadB valueReceiveMVar valueToSendMVar = do
  z <- takeMVar valueReceiveMVar
  putMVar valueToSendMVar (1.2 * fromIntegral z)

main :: IO ()
main = do
  aMVar <- newEmptyMVar
  bMVar <- newEmptyMVar
  forkIO (threadA aMVar bMVar)
  forkIO (threadB aMVar bMVar)
  threadDelay 1000
