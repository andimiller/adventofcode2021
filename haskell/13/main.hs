{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text           ( sepBy1
                                                , parseOnly
                                                , many1
                                                , char
                                                , Parser
                                                , digit
                                                , string
                                                )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import           Control.Applicative            ( (<|>) )
import           Data.Text                      ( pack )
import qualified Data.Set                      as Set
                                                ( Set
                                                , fromList
                                                , map
                                                )
import qualified Data.Foldable                 as Foldable
                                                ( maximum )

-- functions are composed left to right
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f

number :: Parser Int
number = many1 digit <&> read

data Coordinate = Coordinate { xPos :: Int, yPos :: Int } deriving (Show, Eq, Ord)

type Coordinates = Set.Set Coordinate

coord :: Parser Coordinate
coord = do
  x <- number
  char ',' & void
  y <- number
  return (Coordinate x y)

data Fold = YFold Int | XFold Int deriving (Show, Eq)

xfold :: Parser Fold
xfold = string "x=" *> number <&> XFold

yfold :: Parser Fold
yfold = string "y=" *> number <&> YFold

fold :: Parser Fold
fold = string "fold along " *> (xfold <|> yfold)

newline :: Parser ()
newline = void (char '\n')

input :: Parser (Coordinates, [Fold])
input = do
  coords <- coord `sepBy1` newline <&> Set.fromList
  newline
  newline
  folds <- fold `sepBy1` newline
  return (coords, folds)

-- helper to discard eithers
rethrow :: (Show l) => Either l r -> r
rethrow = either (show >>> error) id

-- read the input file
readInput :: IO (Coordinates, [Fold])
readInput = readFile ("input.txt" :: String) <&> pack >>> parseOnly input >>> rethrow

foldYCoord :: Int -> Coordinate -> Coordinate
foldYCoord i (Coordinate x y) | y > i     = Coordinate x (i - (y - i))
                              | otherwise = Coordinate x y


foldXCoord :: Int -> Coordinate -> Coordinate
foldXCoord i (Coordinate x y) | x > i     = Coordinate (i - (x - i)) y
                              | otherwise = Coordinate x y

runFold :: Fold -> Coordinates -> Coordinates
runFold (YFold i) c = Set.map (foldYCoord i) c
runFold (XFold i) c = Set.map (foldXCoord i) c

partOne :: IO ()
partOne = do
  (cs, folds) <- readInput
  let result = runFold (head folds) cs
  print (length result)

printer :: Coordinates -> String
printer cs = unlines do
  yc <- [0 .. (Set.map yPos cs & Foldable.maximum)]
  return do
    xc <- [0 .. (Set.map xPos cs & Foldable.maximum)]
    return if Coordinate xc yc `elem` cs then '#' else '.'

partTwo :: IO ()
partTwo = do
  (cs, folds) <- readInput
  let result = foldl (flip runFold) cs folds
  putStrLn (printer result)

main :: IO ()
main = do
  partOne
  partTwo
