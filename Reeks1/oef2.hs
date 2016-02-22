--Church booleans
cTrue = \x -> \y -> x
cFalse = \x -> \y -> y

--Conversiefuncties
churchToBool = \b -> b True False
boolToChurch = \b -> if b
  then cTrue
  else cFalse

--Operaties
cAnd = \b1 -> \b2 -> b1 (b2 b1)
cOr = \b1 -> \b2 -> b1 (b1 b2)
cNot = \b -> b cFalse cTrue

main = do
  let cBoolA = cAnd cFalse cFalse
  let cBoolB = cOr cTrue cTrue
  let bool = churchToBool cBoolB   --Fout vanaf hier bij elk resultaat van cAnd of cOr, geen problemen bij cNot
  --let bool = churchToBool cTrue --Werkt wel
  print bool
