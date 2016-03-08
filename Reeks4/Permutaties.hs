permutaties :: [a] -> [[a]]
permutaties [] = []
permutaties [a] = [[a]]
permutaties (x:xs) = [ take i p ++ x : drop i p |
                       p <- permutaties xs,
                       i <- [0..length xs] ]
