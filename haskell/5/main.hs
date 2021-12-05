{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Bifunctor                 ( Bifunctor
                                                , bimap
                                                )
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

data Position = Position
  { x :: Int
  , y :: Int
  }
  deriving (Show, Eq, Ord)

data Line = Line
  { from :: Position
  , to   :: Position
  }
  deriving Show

position :: Parser Position
position = do
  x <- decimal
  char ','
  y <- decimal
  return (Position x y)

line :: Parser Line
line = do
  p1 <- position
  string " -> "
  p2 <- position
  return (Line p1 p2)


parseInput :: Parser [Line]
parseInput = line `sepBy1` (char '\n')

-- helper to discard eithers
rethrow :: (Show l) => Either l r -> r
rethrow = either (show >>> error) id

-- read the input file
readInput :: IO [Line]
readInput = readFile ("input.txt" :: String) <&> pack >>> parseOnly parseInput >>> rethrow

range :: Int -> Int -> [Int]
range x y | x > y  = reverse [y .. x]
          | x < y  = [x .. y]
          | x == y = [x]

pointsOnLine :: Line -> [Position]
pointsOnLine (Line (Position x1 y1) (Position x2 y2)) | x1 == x2  = map (Position x1) (range y1 y2)
                                                      | y1 == y2  = map ((flip Position) y1) (range x1 x2)
                                                      | otherwise = []

overlaps :: Ord a => [a] -> [a]
overlaps = sort >>> group >>> filter (length >>> (> 1)) >>> map head

partOne :: IO ()
partOne = do
  lines <- readInput
  let points       = map pointsOnLine lines & concat
  let overlapCount = overlaps points & length
  putStrLn (show overlapCount)

diff :: Num a => a -> a -> a
diff a b = (a - b) & abs

pointsOnLine2 :: Line -> [Position]
pointsOnLine2 (Line (Position x1 y1) (Position x2 y2))
  | x1 == x2                     = map (Position x1) (range y1 y2)
  | y1 == y2                     = map ((flip Position) y1) (range x1 x2)
  | (diff x1 x2) == (diff y1 y2) = zip (range x1 x2) (range y1 y2) & map (uncurry Position)
  | otherwise                    = []

partTwo :: IO ()
partTwo = do
  lines <- readInput
  let points       = map pointsOnLine2 lines & concat
  let overlapCount = overlaps points & length
  putStrLn (show overlapCount)

main :: IO ()
main = do
  partOne
  partTwo
