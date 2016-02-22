import Data.List hiding (tails)

-- Implementeer de grootste gemene deler met het algoritme van Euclides.
-- Beschrijving (wikipedia):
-- bepaal ggd(A, B):
--  als B = 0
--    antwoord = A
--  anders
--    antwoord = ggd(B, (A mod B))
ggd :: Integral a => a -> a -> a
ggd a 0 = a
ggd a b = ggd b (a `mod` b)

-- Ga na of twee getallen relatief priem zijn,
-- twee getallen zijn relatief priem (coprime) als hun grootste gemene deler 1 is.
coprime :: Integral a => a -> a -> Bool
coprime x y = ggd x y == 1

-- Ga na of een getal een priemgetal is
isPriemgetal :: Integral a => a -> Bool
isPriemgetal x = all (coprime x) [1..boundary]
                 where boundary = floor . sqrt . fromIntegral $ x

-- Maak een oneindige lijst van priemgetallen
allePriemgetallen :: Integral a => [a]
allePriemgetallen = filter isPriemgetal [2..]

-- Maak een functie die de de eerste x priemgetallen teruggeeft
geefPriemgetallen :: Int -> [Int]
geefPriemgetallen x = take x allePriemgetallen

-- Bereken het getal Pi met de Leibniz formule
-- De algemene formule is: (-1)^n/(2n+1) = Pi/4
-- 1 - 1/3 + 1/5 - 1/7 + 1/9 ...
-- Het argument i geeft aan hoe precies je Pi wilt berekenen.
-- Hint1: Let op met de syntax in Haskell voor negatieve getallen
-- Hint2: Integers kan je converteren met fromInteger
berekenPi :: Integer -> Double
berekenPi i  = som * 4
               where p = fromInteger i
                     berekening :: Double -> Integer -> Double
                     berekening x y = x + (-1)^fromIntegral y/(2*fromIntegral y+1)
                     som = foldl berekening 0 [0..p]

-- Verdeel een lijst in sublijsten waar elke sublijst
-- opeenvolgende herhaalde elementen bevat.
-- Bijvoorbeeld
-- pack ['a','a' , 'b','b','c','d','d','d' ]
-- > [ ['a','a'], ['b','b'], ['c'], ['d','d','d']  ]
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack l = foldr functie [] l
         where functie x a = case a of [] -> [[x]]
                                       ((y:ys):zs) -> if x == y then (x:y:ys):zs else [x]:(y:ys):zs

-- Implementeer runlength encoding
-- *Main> encode "aabbccddeeffabcd"
-- [(2,'a'),(2,'b'),(2,'c'),(2,'d'),(2,'e'),(2,'f'),(1,'a'),(1,'b'),(1,'c'),(1,'d')]

encode :: String -> [(Integer, Char)]
encode [] = []
encode l = foldr (\x a -> case a of [] -> [(1, x)]
                                    ((n, c):xs) -> if c == x then (n+1, c):xs else (1, x):(n, c):xs) [] l


--Of veel korter:
encode' :: String -> [(Int, Char)]
encode' l = map (\x -> (length x, head x)) (group l)


-- Implementeer een functie die de sublijsten
-- van een lijst samengevoegd in 1 grote lijst
-- Bijvoorbeeld:
--  *Main> flatten [[1,2,3],[3,4,5,3],[12,3,4]]
--  [1,2,3,3,4,5,3,12,3,4]
flatten :: [[a]] -> [a]
flatten = foldr (++) []

--Of nog gemakkelijker
flatten' :: [[a]] -> [a]
flatten' = concat


-- Implementeer runlength decodering
-- Bijvoorbeeld:
-- *Main> decode (encode "aabbcccddd")
-- "aabbcccddd"
-- Hint: je zal hier flatten goed kunnen gebruiken

decode = flatten . map (uncurry replicate)
decode' = flatten . map (\(n, c) -> replicate n c)

-- Gegeven twee indexen extraheer een sublijst aangegeven door deze indexen.
-- Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice l i k  = take (k-i+1) $ drop (i-1) l

-- Genereer alle tails van een lijst
-- Voorbeeld:
-- *Main> tails "abcd"
-- ["abcd","bcd","cd","d",""]
tails :: [a] -> [[a]]
tails [] = []
tails s@(x:xs) = s:tails xs

-- Genereer de sets die je kan vormen door N elementen uit een lijst te kiezen.
-- *Main> combinations 2 "abcdef"
-- ["ab","ac","ad","ae","af","bc","bd","be","bf","cd","ce","cf","de","df","ef"]
-- Hint: Je kan "combinations 1 l"  implementeren met map
combinations :: Ord a => Int -> [a] -> [[a]]
combinations 0 _  = []
combinations _ [] = []
combinations 1 l  = map (:[]) l
combinations n l  = [x:y:ys | x <- l, (y:ys) <- combinations (n-1) l, x < y]
