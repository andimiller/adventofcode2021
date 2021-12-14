{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text           ( sepBy1
                                                , parseOnly
                                                , many1
                                                , char
                                                , Parser
                                                , inClass
                                                , satisfy, string
                                                )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import Data.Maybe (maybeToList)
import           Data.Text                      ( pack
                                                , Text
                                                )
import           Control.Applicative            ( (<|>) )
import           Data.List as List                      ( find, group, sort )
import Data.Tuple as Tuple (swap)

-- functions are composed left to right
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f

letter :: Parser Char
letter = inClass "A-Z" & satisfy

initial :: Parser [Char]
initial = letter & many1

data Rule = Rule { pair :: (Char, Char), insertion :: Char } deriving (Show)

rule :: Parser Rule
rule = do
  l <- letter
  r <- letter
  string " -> "
  i <- letter
  Rule (l, r) i & return

newline :: Parser ()
newline = char '\n' & void

input :: Parser ([Char], [Rule])
input = do
  i <- initial
  newline
  newline
  rs <- rule `sepBy1` newline
  (i, rs) & return
  

-- helper to discard eithers
rethrow :: (Show l) => Either l r -> r
rethrow = either (show >>> error) id

-- read the input file
readInput :: IO ([Char], [Rule])
readInput = readFile ("input.txt" :: String) <&> pack >>> parseOnly input >>> rethrow

evolvePair :: [Rule] -> [Char] -> (Char, Char) -> [Char]
evolvePair rs prev lr @ (l, r) = List.find (pair >>> (== lr)) rs & fmap insertion & maybeToList & (l:) & (prev ++)

evolve :: [Rule] -> [Char] -> [Char]
evolve rs cs = zipWith (,) cs (tail cs) & foldl (evolvePair rs) [] & (++ [last cs])

applyN 0 f a = a
applyN n f a = applyN (n - 1) f (f a)

partOne :: IO ()
partOne = do
  (i, rs) <- readInput
  print i
  let r = applyN 10 (evolve rs) i
  let counts = (sort >>> group >>> fmap length) r
  let score = (maximum counts) - (minimum counts)
  print score

partTwo :: IO ()
partTwo = do
  (i, rs) <- readInput
  print i
  let r = applyN 40 (evolve rs) i
  let counts = (sort >>> group >>> fmap length) r
  let score = (maximum counts) - (minimum counts)
  print score

main :: IO ()
main = do
  partOne
  partTwo
