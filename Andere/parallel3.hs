import Control.Concurrent

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n | n > 1 = fib (n - 1) + fib (n - 2)

relprime :: Int -> Int -> Bool
relprime x y = gcd x y == 1

euler :: Int -> Int
euler n = length (filter (relprime n) (mklist n))

mklist :: Int -> [Int]
mklist n = [1 .. n]

sumEuler :: Int -> Int
sumEuler = sum . map euler . mklist

fibThread :: Int -> MVar Int -> IO ()
fibThread n resultMVar = putMVar resultMVar (fib n)

s1 :: Int
s1 = sumEuler 7450

main :: IO ()
main = do putStrLn "explicit SumFibEuler"
          fibResult <- newEmptyMVar
          _ <- forkIO (fibThread 40 fibResult)
          seq s1 (return ()) -- pseq is hier niet nodig
          f <- takeMVar fibResult
          putStrLn ("sum: " ++ show (s1 + f))
