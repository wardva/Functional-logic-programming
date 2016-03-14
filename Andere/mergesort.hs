sort :: Ord a => [a] -> [a]
sort [] = []
sort [a] = [a]
sort a = let h  = quot (length a) 2
             l1 = sort $ take h a
             l2 = sort $ drop h a
         in merge l1 l2
  where merge a [] = a
        merge [] b = b
        merge a@(x:xs) b@(y:ys)
            | x <  y = x : merge xs b
            | x >= y = y : merge a ys
        merge _ _ = []
