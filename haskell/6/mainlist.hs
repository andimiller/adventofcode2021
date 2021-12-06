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

-- sometimes we want to map a functor to a constant
as :: Functor f => a -> f t -> f a
as a f = fmap (const a) f

fish :: Parser [Int]
fish = decimal `sepBy1` (char ',')

-- helper to discard eithers
rethrow :: (Show l) => Either l r -> r
rethrow = either (show >>> error) id

-- read the input file
readInput :: IO [Int]
readInput = readFile ("input.txt" :: String) <&> pack >>> parseOnly fish >>> rethrow

data AgeBracket = AgeBracket
  { age       :: Int
  , instances :: Int
  }
  deriving (Show, Eq, Ord)

tick :: AgeBracket -> [AgeBracket]
tick (AgeBracket 0 c) = [(AgeBracket 6 c), (AgeBracket 8 c)]
tick (AgeBracket n c) = [(AgeBracket (n - 1) c)]

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f a = a
applyN n f a = applyN (n - 1) f (f a)

combineBrackets :: [AgeBracket] -> AgeBracket
combineBrackets bs = AgeBracket (head bs & age) (map instances bs & sum)

merge :: [AgeBracket] -> [AgeBracket]
merge = sort >>> groupBy ((==) `on` age) >>> map combineBrackets

initialGrouping :: [Int] -> [AgeBracket]
initialGrouping = sort >>> group >>> map (\i -> AgeBracket (head i) (length i))

partOne :: IO ()
partOne = do
  input <- readInput
  let initial = initialGrouping input
  let result  = applyN 80 (map tick >>> mconcat >>> merge) initial
  let count   = map instances result & sum
  putStrLn (show count)

partTwo :: IO ()
partTwo = do
  input <- readInput
  let initial = initialGrouping input
  let result  = applyN 256 (map tick >>> mconcat >>> merge) initial
  let count   = map instances result & sum
  putStrLn (show count)

main :: IO ()
main = do
  partOne
  partTwo
