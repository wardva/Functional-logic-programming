import Control.Concurrent

data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

-- Create a Logger
initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  _ <- forkIO (logger l)
  return l

-- Given a logger
-- read the logcommand
-- and execute it
-- Message => print the message and "loop"
-- Stop    => print "logger: stop"
logger :: Logger -> IO ()
logger (Logger m) = loop
  where loop = do {
    message <- takeMVar m ;
    case message of
      Message s -> putStrLn s >> loop
      Stop s    -> print "logger: stop" >> putMVar s ()
  }


logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s --Door de lege MVar blokkeert dit tot de logger gestopt is!


main :: IO ()
main = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "bye"
  logStop l
