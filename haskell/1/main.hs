{-# LANGUAGE BlockArguments #-}

import           Data.Function                  ( (&) )
import           Text.Read

readInput :: IO [Integer]
readInput = do
  contents <- readFile "input.txt"
  contents & lines & map read & pure


sliding :: Int -> [a] -> [[a]]
sliding n [] = []
sliding n xs = take n xs : sliding n (tail xs) & filter ((n ==) . length)

lessThan :: Ord (a) => [a] -> Bool
lessThan [a, b] = a < b

partOne :: IO ()
partOne = do
  ints <- readInput
  ints & sliding 2 & filter lessThan & length & show & putStrLn

partTwo :: IO ()
partTwo = do
  ints <- readInput
  ints
    & sliding 3
    & map sum
    & sliding 2
    & filter lessThan
    & length
    & show
    & putStrLn


main :: IO ()
main = do
  partOne
  partTwo
