data Move = Rock | Paper | Scissors deriving(Eq)
data Result = Won | Lost | Tie deriving(Show)

play :: Move -> Move -> Result
play Paper Rock = Won
play Scissors Paper = Won
play Rock Scissors = Won
play a b
    | a == b    = Tie
    | otherwise = Lost
