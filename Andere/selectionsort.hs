sort :: Ord a => [a] -> [a]
sort [] = []
sort [a] = [a]
sort xs = let x = maximum xs in sort (remove x xs) ++ [x]
  where remove _ [] = []
        remove a (x:xs)
            | x == a = xs
            | otherwise = x : remove a xs
