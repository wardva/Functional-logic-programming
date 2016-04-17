pairs :: Int -> [(Int, Int)]
pairs n = [ (x, y) | x <- [0..n], y <- [x+1..n] ]

pairs' :: Int -> [(Int, Int)]
pairs' n = [0..n] >>= (\x -> [x+1..n] >>= \y -> return (x, y))

pairs'' :: Int -> [(Int, Int)]
pairs'' n = do
  x <- [0..n]
  y <- [x+1..n]
  return (x, y)
