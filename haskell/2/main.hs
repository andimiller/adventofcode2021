{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text
import           Control.Applicative
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import           Data.Monoid
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Text.Read

data Move
  = Up Int
  | Down Int
  | Forward Int
  deriving (Show)

data Position = Position
  { horizontal :: Int
  , depth      :: Int
  , aim        :: Int
  }
  deriving Show

movementParser :: Text -> (Int -> Move) -> Parser Move
movementParser t m = do
  string t
  char ' '
  decimal <&> m

parseMove :: Parser Move
parseMove = movementParser "up" Up <|> movementParser "down" Down <|> movementParser "forward" Forward

readInput :: IO (Either String [Move])
readInput = readFile ("input.txt" :: String) <&> lines <&> map pack <&> traverse (parseOnly parseMove)

move :: Move -> Endo Position
move (Up      n) = Endo \p -> p { depth = (depth p) - n }
move (Down    n) = Endo \p -> p { depth = (depth p) + n }
move (Forward n) = Endo \p -> p { horizontal = (horizontal p) + n }

run :: [Move] -> Position -> Position
run ms = ms & map move & mconcat & appEndo


partOne :: IO ()
partOne = do
  eitherMoves <- readInput
  let position = eitherMoves <&> run <&> \f -> f (Position 0 0 0)
  let answer   = position <&> \p -> (horizontal p) * (depth p)
  putStrLn (show answer)

move2 :: Move -> Endo Position
move2 (Down    n) = Endo \p -> p { aim = (aim p) + n }
move2 (Up      n) = Endo \p -> p { aim = (aim p) - n }
move2 (Forward n) = Endo \p -> p { horizontal = (horizontal p) + n, depth = (depth p) + ((aim p) * n) }

run2 :: [Move] -> Position -> Position
run2 ms = ms & map move2 & reverse & mconcat & appEndo

partTwo :: IO ()
partTwo = do
  eitherMoves <- readInput
  let position = eitherMoves <&> run2 <&> \f -> f (Position 0 0 0)
  let answer   = position <&> \p -> (horizontal p) * (depth p)
  putStrLn (show position)
  putStrLn (show answer)


main :: IO ()
main = do
  partOne
  partTwo
