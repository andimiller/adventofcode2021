{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Bifunctor                 ( Bifunctor
                                                , bimap
                                                )
import           Data.Foldable                  ( foldMap )
import           Data.Function                  ( (&) )
import           Data.Function
import           Data.Functor                   ( (<&>) )
import           Data.List                     as List
import           Data.Maybe                     ( isJust )
import           Data.Monoid
import           Data.Semigroup
import           Data.Set                      as Set
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Debug.Trace
import           Text.Read

-- functions are composed left to right
f >>> g = g . f


signalChar :: Parser Char
signalChar = satisfy ((flip member) (Set.fromList ['a', 'b', 'c', 'd', 'e', 'f', 'g']))

signal :: Parser (Set Char)
signal = fmap Set.fromList (many1 signalChar)

signals :: Parser [Set Char]
signals = signal `sepBy1` (char ' ')

line :: Parser ([Set Char], [Set Char])
line = do
  i <- signals
  string " | "
  o <- signals
  return (i, o)

input :: Parser [([Set Char], [Set Char])]
input = line `sepBy1` (char '\n')

-- helper to discard eithers
rethrow :: (Show l) => Either l r -> r
rethrow = either (show >>> error) id

-- read the input file
readInput :: IO [([Set Char], [Set Char])]
readInput = readFile ("input.txt" :: String) <&> pack >>> parseOnly input >>> rethrow

guessNumber :: Set Char -> Maybe Int
guessNumber cs | length cs == 2 = Just 1
               | length cs == 4 = Just 4
               | length cs == 3 = Just 7
               | length cs == 7 = Just 8
               | otherwise      = Nothing

partOne :: IO ()
partOne = do
  i <- readInput
  let result = List.map (snd >>> List.map guessNumber) i
  let count  = List.filter isJust (concat result) & length
  putStrLn (show count)


findN :: Int -> [Set Char] -> Maybe (Set Char)
findN 1 cs = find (\x -> (length x) == 2) cs
findN 4 cs = find (\x -> (length x) == 4) cs
findN 7 cs = find (\x -> (length x) == 3) cs
findN 8 cs = find (\x -> (length x) == 7) cs

minus :: Ord a => Set a -> Set a -> Set a
minus = Set.difference

remove :: Ord a => Set a -> a -> Set a
remove = flip (Set.delete)

contains = Set.isSubsetOf

solve :: [(Set Char)] -> Maybe [(Int, Set Char)]
solve cs = do
  one   <- findN 1 cs
  four  <- findN 4 cs
  seven <- findN 7 cs
  eight <- findN 8 cs
  let cs' = (Set.fromList cs) `minus` (Set.fromList [one, four, seven, eight])
  let defg = eight `minus` seven
  six <- find (contains defg) cs'
  let cs'' = cs' `remove` six
  let c = (six `minus` (eight `minus` seven)) `minus` (seven `minus` one)
  two <- find (contains c >>> not) cs''
  let cs'''    = cs'' `remove` two
  let f        = (four `minus` one) `minus` two
  let g        = (four `minus` one) `minus` f
  let filtered = (Set.filter (\x -> (length x) == 5) cs''')
  three <- find (contains f >>> not) filtered
  let cs'''' = cs''' `remove` three
  zero <- List.find (contains g >>> not) cs''''
  let cs''''' = cs'''' `remove` zero
  let b = two `minus` ((eight `minus` seven) `minus` (seven `minus` one))
  nine <- find (contains b) cs'''''
  five <- find (contains b >>> not) cs'''''
  return [(0, zero), (1, one), (2, two), (3, three), (4, four), (5, five), (6, six), (7, seven), (8, eight), (9, nine)]

readAnswer :: [(Int, Set Char)] -> Set Char -> Maybe Text
readAnswer lookup target = find (snd >>> (== target)) lookup <&> fst >>> show >>> pack


solver :: [Set Char] -> [Set Char] -> Maybe Int
solver left right = do
  lookup  <- solve left
  letters <- traverse (readAnswer lookup) right
  let body = mconcat letters
  return ((unpack >>> read) body)


partTwo :: IO ()
partTwo = do
  i <- readInput
  let results = traverse (uncurry solver) i
  let total   = fmap sum results
  putStrLn (show total)

main :: IO ()
main = do
  partOne
  partTwo
