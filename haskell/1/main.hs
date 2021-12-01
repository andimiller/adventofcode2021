{-# LANGUAGE BlockArguments #-}

import           Data.Function                  ( (&) )
import           Text.Read

readInput :: IO [Integer]
readInput = do
  content <- readFile "input.txt"
  let ls      = lines content
  let results = map read ls :: [Integer]
  pure results

sliding :: Int -> [a] -> [[a]]
sliding n xs | length xs < n = []
             | otherwise     = take n xs : sliding n (tail xs)

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
  pure ()
