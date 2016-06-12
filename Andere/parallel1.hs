import Control.Parallel

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

sumFibEuler :: Int -> Int -> Int
sumFibEuler a b = fib a + sumEuler b

--Slechte versie, f wordt parallel geëvalueerd met (f + e)
--maar de evaluatievolgorde van (f + e) is niet bepaald.
--Alles is mogelijk, geen garantie dat f en e tegelijk
--geëvalueerd worden.
parSumFibEuler :: Int -> Int -> Int
parSumFibEuler a b = f `par` (f + e)
  where
    f = fib a
    e = sumEuler b

parSumFibEuler' :: Int -> Int -> Int
parSumFibEuler' a b = f `par` (e `pseq` (e + f))
  where
    f = fib a
    e = sumEuler b

mapFib :: [Int]
mapFib = map fib [37, 38, 39, 40]

mapEuler :: [Int]
mapEuler = map sumEuler [7600, 7600]

parMapFibEuler :: Int
parMapFibEuler = a `par` (b `pseq` total)
  where
    a = mapFib
    b = mapEuler
    total = sum a + sum b

forceList :: [a] -> ()
forceList = foldr pseq ()

main :: IO ()
main = print $ euler 30
