--Voorlaatste element van een lijst
voorlaatste l = last (init l)

--Nde element uit lijst
geefNde n l =  l !! n

--Nagaan of lijst palindroom is
palindroom l = l == reverse l

--Elk element van lijst verdubbelen: [1,2,3] -> [1,1,2,2,3,3]
verdubbel l =  concat [[x,x] | x <- l ]

--Twee lijsten in elkaar ritsen tot lijst van tuples.
-- : (dubbelpunt) is de prefix operator.
-- Deze oplossing: zelfde grootte is vereist
rits [] [] = []
rits (x:xs) (y:ys) = (x,y) : rits xs ys

-- Zelfde groote niet vereist
rits2 xs ys = [(xs!!i, ys!!i) | i <-[0..min (length xs) (length ys) - 1]]

--Splits een lijst in een koppel van twee lijsten, eerste deel alle elementen
--met index < k, tweede deel index >= k
splits k l = ([l!!i | i <- [0..k-1]], [l!!i | i <- [k..length l-1]])

--Verwijder het element op index k
--verwijderK k l = [l!!i | i <- [0..k-1]] ++ [l!!i | i <- [k+1..length l-1]]
--Alternatief met drop en take:
--drop n xs: returnt lijst zonder eerste n elementen
--take n xs: returnt eerste n elementen van lijst
verwijderK k l = take k l ++ drop (k+1) l

--N keer roteren naar links
roteer n l = drop n l ++ take n l

main = do
  let l = [0,1,2,3,4,5,6,7]
  let c = ['a', 'b', 'c', 'd', 'e', 'f']
  let item1 = voorlaatste l
  let item2 = geefNde 5 l
  let bool = palindroom l
  let dubbel = verdubbel l
  let geritst = rits2 l c
  let nieuw = verwijderK 4 l
  let geroteerd = roteer 2 l
  print geroteerd
