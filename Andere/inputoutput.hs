main :: IO ()
main = putChar 'A' >>= \x -> putChar ' ' >>= \y -> putStr "car"

main' :: IO ()
main' = putChar 'A' >> putChar ' ' >> putStr "car"

putStr' :: String -> IO()
putStr' (x:xs) = putChar x >> putStr' xs

--foldr1 :: Foldable t => (a -> a -> a) -> t a -> a
putStr'' :: String -> IO()
putStr'' s = foldr1 (>>) (map putChar s)

test = let x = print "ha" in x; x
