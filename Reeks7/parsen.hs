import Text.ParserCombinators.Parsec
import Data.CSV

main = do
  result <- parseFromFile csvFile "HSK1.csv"
  putStr "Hello world"
