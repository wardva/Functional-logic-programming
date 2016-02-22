
zero = \f -> \x -> x
one = \f -> \x -> f x
two = \f -> \x -> f (f x)

--church naar nummer
getNum church = church(\x -> (x + 1)) 0
--nummer naar church notation
numToChurch 0 = zero
numToChurch num = \f -> \x -> (numToChurch (num - 1) f (f x))
--1 optellen bij churchone getal
increment cNum = \f -> \x -> f (cNum f x )
--2 church getallen optellen
add cNumA cNumB = \f -> \x -> cNumA f (cNumB f x)
--2 church getallen vermenigvuldigen
mul cNumA cNumB = \f -> \x -> cNumA (cNumB f) x

main = do
  let x = numToChurch 5   --5
  let y = increment x     --6
  let z = add y two       --8
  let q = mul z two       --16
  let num = getNum q      --16
  print num
  --print num
