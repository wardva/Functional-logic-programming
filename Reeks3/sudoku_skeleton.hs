import Data.List
import Data.Maybe
import Text.Printf
import Data.Sequence (fromList)
--   _________         .___      __
--  /   _____/__ __  __| _/____ |  | ____ __
--  \_____  \|  |  \/ __ |/  _ \|  |/ /  |  \
--  /        \  |  / /_/ (  <_> )    <|  |  /
-- /_______  /____/\____ |\____/|__|_ \____/
--        \/           \/           \/
-- Functional Programming Practice Set 3
-- Christophe.Scholliers@UGent.be

-- We represent boards like shown below

------ 0  1  2   3  4  5   6  7  8
--0 [[-1,-1,-1|,-1,-1,-1,|-1,-1,-1],
--1  [-1,-1,-1|,-1,-1,-1,|-1,-1,-1],
--2  [-1,-1,-1|,-1,-1,-1,|-1,-1,-1],
------------------------------------
--3  [-1,-1,-1|,-1,-1,-1,|-1,-1,-1],
--4  [-1,-1,-1|,-1,-1,-1,|-1,-1,-1],
--5  [-1,-1,-1|,-1,-1,-1,|-1,-1,-1],
-------------------------------------
--6  [-1,-1,-1|,-1,-1,-1,| -1,-1,-1],
--7  [-1,-1,-1|,-1,-1,-1,| -1,-1,-1],
--8  [-1,-1,-1|,-1,-1,-1,| -1,-1,-1]]

-- For example this is an example sudoku
testsudoku = [[ 9,-1,-1,-1,-1,-1,-1,-1,-1],
               [ 3,-1, 4,-1,-1, 1,-1,-1,-1],
               [ 8,-1,-1,-1,-1, 3, 7, 6, 2],
               [-1, 4,-1,-1, 6,-1, 2, 7, 1],
               [ 1, 8, 6, 9, 7, 2, 4, 5, 3],
               [ 7, 3, 2, 5, 1, 4, 6, 9, 8],
               [ 4, 1, 3, 8, 5, 6, 9, 2, 7],
               [ 6, 7, 8, 2, 4, 9, 3, 1, 5],
               [ 2, 9, 5, 1, 3, 7, 8, 4,-1]]
              :: Board

type Board = [[Int]]
empty      = (-1)::Int
width      =  9
height     =  9

emptyRow   = replicate width  empty
emptyBoard = replicate height emptyRow

-- Get a row from the board
getRow :: Board -> Int -> [Int]
getRow b n = b !! n
-- Get a column from the board
getCol :: Board -> Int -> [Int]
getCol b n = map (!!n) b
-- Check whether a list contains a number
contains :: Int -> [Int] -> Bool
contains n l = n `elem` l
-- Get a sublist of a list
getSubList :: Int -> Int -> [a] -> [a]
getSubList s e l = take (e - s + 1) (drop s l)

------------------------------------------------
-- The following three functions will
-- check whether the square given by the
-- coordinates x and y contains a number n
-- For example on the following board
-- given the coordinate (1,1) we need to check
-- the rectangle [(0,0);(2,2)]
-- and for the coordinate (4,1) we need to check
-- the rectangle [(0,3);(2,5)]
------ 0  1  2   3  4  5   6  7  8
--0 [[1, 3,  2|,-1,-1,-1,|-1,-1,-1],
--1  [1, 4,  7|,-1,-1,-1,|-1,-1,-1],
--2  [1, 5,  6|,-1,-1,-1,|-1,-1,-1],

-- We first define a function which gives us the
-- index for a single axis
-- for example 1 -> (0,2)
index :: Int -> (Int,Int)
index n = (begin, begin + 2)
          where begin = floor (fromIntegral n / 3) * 3

-- We collect all the numbers in the subsquare given
-- by the coordinates (x,y)
-- in the above example this would be [1,3,2,1,4,7,1,5,6]
-- Hint: The whole board is a list of list
-- so we can just take a sublist of the board to get all the rows.
-- These rows still need to be cut on the right columns
-- this again is taking a sublist.
getSubSquare :: Board -> Int -> Int -> [Int]
getSubSquare b x y = concatMap (getCol rows) [(fst yrange)..(snd yrange)]
                     where xrange = index x
                           yrange = index y
                           rows = map (getCol b) [(fst xrange)..(snd xrange)]

-- Now that we have defined all the helper functions
-- it is easy to define the function which takes a board,
-- a number, a coordinate and determines whether this number
-- is in the subsquare defined by the coordinate
containedInSquare :: Board -> Int -> Int -> Int -> Bool
containedInSquare b n x y = n `elem` square
                            where square = getSubSquare b x y

------------------------------------------------
-- check whether it is possible to place
-- the number n on the position x and y
-- this means that the number is:
-- not in the same row, not in the same column and not in the subsquare
canPlaceNumber :: Board -> Int -> Int -> Int -> Bool
canPlaceNumber board n x y = not (containedInSquare board n x y) &&
                             notElem n (getRow board y) &&
                             notElem n (getRow board x)

-- Replace an element in a list
replace :: a -> Int -> [a] -> [a]
replace n x l = take x l ++ n : drop (x+1) l

-- Update a number on the board
update :: Int -> Int -> Int -> Board -> Board
update n x y b = replace (replace n x (b !! y)) y b

-- Find the first empty element on the board
findFirstEmpty :: Board -> (Int,Int)
findFirstEmpty b = head [(fst n, i) | (i, l) <- c, let ll = zip [0..] l, n <- ll, snd n == -1]
                   where c = zip [0..] b


-- Check whether there is an empty space on the board
notEmpty :: Board -> Bool
notEmpty b = (-1) `elem` concat b

-- Generate all the numbers which can be placed at a particular location
-- on the board
options :: Board -> Int -> Int -> [Int]
options b x y = [n | n <- [0..9], canPlaceNumber b x y n]

-- Generate the next boards for a given board
nextBoards :: Board -> [Board]
nextBoards b = undefined

-- We found the real solution
solve :: Board -> [Board] -> Board
solve focus options = undefined

-- We hit a dead path try the next option
solve focus options = undefined

-- We are solving the focus, generate the next boards and save the rest in the options
solve focus options = undefined

--  _   _      _                    __                  _   _
-- | | | |    | |                  / _|                | | (_)
-- | |_| | ___| |_ __   ___ _ __  | |_ _   _ _ __   ___| |_ _  ___  _ __  ___
-- |  _  |/ _ \ | '_ \ / _ \ '__| |  _| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
-- | | | |  __/ | |_) |  __/ |    | | | |_| | | | | (__| |_| | (_) | | | \__ \
-- \_| |_/\___|_| .__/ \___|_|    |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
--              | |
--              |_|
-- These functions help for debugging
getX (-1)  =  "_"
getX  x    =  show x

showBoard :: Board -> IO ()
showBoard b = do mapM (\r -> (mapM (putStr . (++ " ") . getX ) r) >>= (\_ -> putStr "\n") ) b
                 return ()
