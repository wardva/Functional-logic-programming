--Church booleans
cTrue = \x -> \y -> x
cFalse = \x -> \y -> y

--Conversiefuncties
churchToBool = \b -> b True False
boolToChurch = \b -> if b
  then cTrue
  else cFalse

--Operaties
cAnd = \b1 -> \b2 ->
cOr = \b1 -> \b2 ->
cNot = \b1 -> \b2 ->

main = do
  let cBool = boolToChurch True
  let bool = churchToBool cBool
  print bool
  --print num
