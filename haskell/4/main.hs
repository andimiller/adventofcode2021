{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Bifunctor                 ( bimap )
import           Data.Function                  ( (&) )
import           Data.Function
import           Data.Functor                   ( (<&>) )
import           Data.List
import           Data.Monoid
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Text.Read

-- functions are composed left to right
f >>> g = g . f

-- sometimes we want to map a functor to a constant
as :: Functor f => a -> f t -> f a
as a f = fmap (const a) f

-- define some bingo types
data Cell =
  Marked |
  Unmarked Int deriving (Show)

isMarked :: Cell -> Bool
isMarked Marked       = True
isMarked (Unmarked _) = False

type Board = [[Cell]]

comma = char ','

-- parse a list of numbers
listOfNumbers = sepBy decimal (char ',')

oneSpace = char ' '
newLine = char '\n'

-- parse a bingo board
cell = decimal <&> Unmarked
bingo = ((skipMany oneSpace) *> cell `sepBy1` (skipMany oneSpace)) `sepBy1` newLine

data Inputs = Inputs
  { inputs :: [Int]
  , boards :: [Board]
  }
  deriving Show

parseInput :: Parser Inputs
parseInput = do
  inputs <- listOfNumbers
  endOfLine
  endOfLine
  bingos <- bingo `sepBy1` (skipMany1 newLine)
  return (Inputs inputs bingos)

hasWonVertically :: Board -> Bool
hasWonVertically = any (all isMarked)
hasWonHorizontally :: Board -> Bool
hasWonHorizontally = transpose >>> hasWonVertically

hasWon :: Board -> Bool
hasWon b = hasWonVertically b || hasWonHorizontally b


mark :: Int -> Cell -> Cell
mark i Marked = Marked
mark i (Unmarked n) | n == i    = Marked
                    | otherwise = Unmarked n

markBoard :: Int -> Board -> Board
markBoard i b = map (map (mark i)) b

play :: [Int] -> [Int] -> [Board] -> ([Int], [Board])
play played [] boards = (played, boards)
play played (h : t) boards | any (hasWon) boards = (played, boards)
                           | otherwise           = play (h : played) t (map (markBoard h) boards)

-- how to turn a cell into points
points Marked       = 0
points (Unmarked i) = i

score :: [Int] -> [Board] -> Maybe Int
score (finalNumber : _) boards = do
  board <- find hasWon boards
  return (board & concat >>> map points >>> sum) <&> (* finalNumber)

-- helper to discard eithers
rethrow :: (Show l) => Either l r -> r
rethrow = either (show >>> error) id

-- read the input file
readInput :: IO Inputs
readInput = readFile ("input.txt" :: String) <&> pack >>> parseOnly parseInput >>> rethrow

partOne :: IO ()
partOne = do
  Inputs inputs boards <- readInput
  let (played, finalBoards) = play [] inputs boards
  let result                = score played finalBoards
  putStrLn (show result)

play2 :: [Int] -> [Int] -> [(Int, Board)] -> [Board] -> [(Int, Board)]
play2 played []      winners boards = winners
play2 played (h : t) winners boards = (uncurry (play2 (h : played) t))
  ((map (markBoard h) boards) & partition hasWon >>> bimap (map (\w -> (h, w))) id >>> bimap (++ winners) id)

score2 :: Int -> Board -> Int
score2 i b = (b & concat >>> map points >>> sum) * i

partTwo :: IO ()
partTwo = do
  Inputs inputs boards <- readInput
  let (winner : _) = play2 [] inputs [] boards
  let result       = (uncurry score2) winner
  putStrLn (show result)

main :: IO ()
main = do
  partOne
  partTwo
