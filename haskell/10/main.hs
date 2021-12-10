{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative            ( (<|>) )
import           Data.Attoparsec.Text           ( Parser
                                                , char
                                                , endOfInput
                                                , many1
                                                , parse
                                                , parseOnly
                                                , sepBy1
                                                )
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                )
import           Data.Function                  ( (&) )
import           Data.List                     as List
                                                ( foldl
                                                , sort
                                                )
import           Data.Text                      ( pack )
import           Data.String.Interpolate        ( i )
import           Control.Monad.Loops            ( iterateUntilM )
import           Data.Either                    ( partitionEithers )
import           Data.Maybe                     ( catMaybes )

-- functions are composed left to right
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f

data Bracket = Curly | Square | Rounded | Triangle deriving (Show, Eq, Ord)
data Action = Open | Close deriving (Show, Eq)
data Token = Token Action Bracket
  deriving (Show, Eq)

token :: Parser Token
token =
  char '{'
    $>  Token Open Curly
    <|> char '['
    $>  Token Open Square
    <|> char '('
    $>  Token Open Rounded
    <|> char '<'
    $>  Token Open Triangle
    <|> char '}'
    $>  Token Close Curly
    <|> char ']'
    $>  Token Close Square
    <|> char ')'
    $>  Token Close Rounded
    <|> char '>'
    $>  Token Close Triangle

line :: Parser [Token]
line = many1 token


-- helper to discard eithers
rethrow :: (Show l) => Either l r -> r
rethrow = either (show >>> error) id

-- read the input file
readInput :: IO [[Token]]
readInput = readFile ("input.txt" :: String) <&> pack >>> parseOnly (line `sepBy1` char '\n') >>> rethrow

score :: Bracket -> Int
score Rounded  = 3
score Square   = 57
score Curly    = 1197
score Triangle = 25137

counter :: ([Bracket], [Token]) -> Either (Maybe Bracket) ([Bracket], [Token])
counter (bs, Token Open b : t ) = Right (b : bs, t)
counter ([], Token Close b : _) = Left (Just b)
counter (bh : bt, Token Close b : tt) | bh == b   = Right (bt, tt)
                                      | otherwise = Left (Just b)
counter ([]     , []) = Right ([], [])
counter (bh : bt, []) = Left Nothing

partOne :: IO ()
partOne = do
  inputs <- readInput
  let (failures, _) = partitionEithers (map (\i -> iterateUntilM (== ([], [])) counter ([], i)) inputs)
  let errors        = catMaybes failures
  let finalScore    = map score errors & sum
  print finalScore

complete :: ([Bracket], [Token]) -> Either (Maybe [Bracket]) ([Bracket], [Token])
complete (bs, Token Open b : t ) = Right (b : bs, t)
complete ([], Token Close b : _) = Left Nothing
complete (bh : bt, Token Close b : tt) | bh == b   = Right (bt, tt)
                                       | otherwise = Left Nothing
complete ([]     , []) = Right ([], [])
complete (bh : bt, []) = Left (Just (bh : bt))

value :: Bracket -> Int
value Rounded  = 1
value Square   = 2
value Curly    = 3
value Triangle = 4

score2 :: [Bracket] -> Int
score2 = map value >>> List.foldl (\i v -> (i * 5) + v) 0

median :: [a] -> a
median as = as !! (length as `div` 2)

partTwo :: IO ()
partTwo = do
  inputs <- readInput
  let (failures, _) = partitionEithers (map (\i -> iterateUntilM (== ([], [])) complete ([], i)) inputs)
  let errors        = catMaybes failures
  let scores        = map score2 errors & List.sort
  let winner        = median scores
  print winner

main :: IO ()
main = do
  partOne
  partTwo
