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
import           Data.Semigroup
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Text.Read

-- functions are composed left to right
f >>> g = g . f

crabs :: Parser [Integer]
crabs = decimal `sepBy1` (char ',')

-- helper to discard eithers
rethrow :: (Show l) => Either l r -> r
rethrow = either (show >>> error) id

-- read the input file
readInput :: IO [Integer]
readInput = readFile ("input.txt" :: String) <&> pack >>> parseOnly crabs >>> rethrow

range :: [Integer] -> [Integer]
range xs = [(minimum xs) .. (maximum xs)]

difference :: Integer -> Integer -> Integer
difference a b = abs (a - b)

run :: [Integer] -> [Integer]
run xs = do
  target <- range xs
  return (sum (map (difference target) xs))

partOne :: IO ()
partOne = do
  input <- readInput
  let result = run input & minimum
  putStrLn (show result)

cumulativeSum :: Integer -> Integer
cumulativeSum 0 = 0
cumulativeSum n = n + (cumulativeSum (n - 1))

fuel :: Integer -> Integer -> Integer
fuel a b = cumulativeSum (difference a b)

run2 :: [Integer] -> [Integer]
run2 xs = do
  target <- range xs
  return (sum (map (fuel target) xs))

partTwo :: IO ()
partTwo = do
  input <- readInput
  let result = run2 input & minimum
  putStrLn (show result)

main :: IO ()
main = do
  partOne
  partTwo
