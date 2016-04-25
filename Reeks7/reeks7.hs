import Text.ParserCombinators.Parsec
import System.Random
import Control.Monad.Trans
import Data.CSV
import Control.Monad
import Control.Monad.State
import System.IO

--Type Question: paar met vraag en antwoord string.
type Question = (String, String)
--Type Questions: Lijst met alle vragen
type Questions = [Question]
--Type Stats: bevat de huidige statistieken (=State)
data Stats = Stats { correct :: Int,
                     incorrect :: Int,
                     total :: Int  }

--Type Card: flashcard met een vraag als String, 5 mogelijke antwoorden
--en de in de index van het juiste antwoord.
data Card = Card { question :: String,
                   answers :: [String],
                   correctAnswer :: Int }

--Deze functie toont een flashcard, leest en controleert het antwoord, en past
--de state aan.
nextQuestion :: Questions -> Stats -> StateT Stats IO ()
nextQuestion qs s = do
  card <- lift $ randomCard qs
  lift $ putStrLn (question card) >> putStrLn ""
  lift $ mapM_ (\i -> putStrLn (show (i+1) ++ ") " ++ answers card !! i)) [0..4]
  answer <- lift readInt
  put $ answered (answer - 1 == correctAnswer card) s

--Deze functie zorgt ervoor dat het programma blijft draaien: de state
--wordt opgevraagd, er wordt een vraag getoond, er worden eventueel
--statistieken geprint. Vervolgens een recursieve call.
start :: Questions -> StateT Stats IO ()
start qs = do
  s <- get
  nextQuestion qs s
  when (total s `mod` 20 == 0) printStats
  start qs

--Print de statistieken.
printStats :: StateT Stats IO ()
printStats = do
  s <- get
  lift $ putStrLn $ "Total: " ++ show (total s)
  lift $ putStrLn $ "Correct: " ++ show (correct s)
  lift $ putStrLn $ "Incorrect: " ++ show (incorrect s)

--Wijzig de statistieken afhanelijk van een bool die aanduidt of een vraag
--juist of correct is beantwoord.
answered :: Bool -> Stats -> Stats
answered b s = Stats { correct = if b then 1 + correct s else correct s,
                       incorrect = if b then correct s else 1 + correct s,
                       total = total s + 1 }
--Lees een Int van stdin.
readInt :: IO Int
readInt = do
  hFlush stdout
  inp <- getLine
  return (read inp)

--Return een lijst van n random waarden in een IO monad.
getNRandom :: (Int,Int) -> Int -> IO [Int]
getNRandom r n = mapM (const (randomRIO r)) [0..(n-1)]

--Returnt de vraagstring van een Question
getQuestion :: Question -> String
getQuestion = fst

--Returnt de antwoordstring van een Question
getAnswer :: Question -> String
getAnswer = snd

--Returnt een willekeurige flashcard
randomCard :: Questions -> IO Card
randomCard qs = do
  let n = length qs
  r <- getNRandom (0, n-1) 5
  c <- randomRIO (0, 4)
  let sel = map (\i -> qs !! i) r
  let q = getQuestion $ sel !! c
  let as = map getAnswer sel
  return Card  { question = q,
                 correctAnswer = c,
                 answers = as }

--Main functie
main :: IO ()
main = do
  file <- parseFromFile csvFile "HSK1.csv"
  let qs = case file of Right x  -> map (\q -> (head q, q !! 1)) x
                        Left _   -> []
  let stats = Stats { correct = 0,
                      incorrect = 0,
                      total = 0 }
  runStateT (start qs) stats
  return ()
