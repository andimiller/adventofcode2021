{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text
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

-- parse boolean numbers
bool = (char '0' & as False) <|> (char '1' & as True)
bools = many bool

-- helper to discard eithers
rethrow :: (Show l) => Either l r -> r
rethrow = either (show >>> error) id

-- read the input file
readInput :: IO (Either String [[Bool]])
readInput = readFile ("input.txt" :: String) <&> lines >>> map pack >>> traverse (parseOnly bools)

-- finds the most common element, solves ties with sort
mostCommon :: Ord a => [a] -> a
mostCommon = sort >>> group >>> sortBy (compare `on` length) >>> last >>> head

boolToInt True  = 1
boolToInt False = 0

render :: [Bool] -> String
render = map (boolToInt >>> show) >>> mconcat

powersOfTwo = map (2 ^) [0 ..]

bitsToDecimal :: [Bool] -> Int
bitsToDecimal = reverse >>> map boolToInt >>> zip powersOfTwo >>> map (uncurry (*)) >>> sum

partOne :: IO ()
partOne = do
  r <- readInput <&> rethrow
  let r'      = r & transpose >>> map mostCommon
  let gamma   = bitsToDecimal r'
  let epsilon = bitsToDecimal (map not r')
  let result  = gamma * epsilon
  putStrLn (show result)

-- methods for part two

oxygenFilter :: Int -> [[Bool]] -> Bool
oxygenFilter n = transpose >>> (!! n) >>> mostCommon

co2Filter :: Int -> [[Bool]] -> Bool
co2Filter n xs = oxygenFilter n xs & not

runner :: (Int -> [[Bool]] -> Bool) -> Int -> [[Bool]] -> [[Bool]]
runner f n xs | xs == []               = error "you cannot use an empty list"
              | (length xs) == 1       = xs
              | n > (length (head xs)) = xs
              | otherwise              = runner f (n + 1) (filter ((!! n) >>> (== (f n xs))) xs)

partTwo :: IO ()
partTwo = do
  r <- readInput <&> rethrow
  let oxygen = (runner oxygenFilter 0 r) & head >>> bitsToDecimal
  let co2 = (runner co2Filter 0 r) & head >>> bitsToDecimal
  let result = oxygen * co2
  putStrLn (show result)

main :: IO ()
main = do
  partOne
  partTwo
