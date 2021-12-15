{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}



import           Data.Attoparsec.Text           ( sepBy1
                                                , parseOnly
                                                , many1
                                                , char
                                                , Parser
                                                , digit
                                                , double
                                                )
import           Data.Char                      ( digitToInt )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>)
                                                , ($>)
                                                )
import           Data.Matrix                   as Matrix
                                                ( fromLists
                                                , Matrix
                                                , safeGet
                                                , setElem
                                                , unsafeGet
                                                , nrows
                                                , ncols
                                                , unsafeSet
                                                , (!)
                                                , mapPos
                                                )
import           Data.Text                      ( pack )
import           Data.Maybe                     ( maybe
                                                , catMaybes
                                                , fromMaybe
                                                , isNothing
                                                , maybeToList
                                                )
import           Data.Monoid                    ( Endo(..)
                                                , mconcat
                                                , appEndo
                                                , mempty
                                                )
import           Data.List                      ( sort )
import           Control.Monad.Loops            ( unfoldM )
import           Debug.Trace                    ( traceShowId )
import qualified Data.Map                      as Map
                                                ( Map(..)
                                                , toList
                                                , adjust
                                                , fromList
                                                , insert
                                                )

-- functions are composed left to right
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f


line :: Parser [Int]
line = many1 (digit <&> digitToInt)

input :: Parser [[Int]]
input = line `sepBy1` char '\n'

-- helper to discard eithers
rethrow :: (Show l) => Either l r -> r
rethrow = either (show >>> error) id

-- read the input file
readInput :: IO (Matrix Int)
readInput = readFile ("input.txt" :: String) <&> pack >>> parseOnly input >>> rethrow >>> Matrix.fromLists

data Node = Visited Int | Tentative Int | Unvisited deriving (Show, Eq, Ord)

instance Semigroup Node where
  (<>) = min

instance Monoid Node where
  mempty = Unvisited

neighbours :: Matrix Int -> (Int, Int) -> [(Int, Int)]
neighbours m (x, y) = catMaybes
  [ Matrix.safeGet (x - 1) y m $> (x - 1, y)
  , Matrix.safeGet (x + 1) y m $> (x + 1, y)
  , Matrix.safeGet x (y - 1) m $> (x, y - 1)
  , Matrix.safeGet x (y + 1) m $> (x, y + 1)
  ]

positions :: Matrix a -> [(Int, Int)]
positions m = do
  r <- [1 .. Matrix.nrows m]
  c <- [1 .. Matrix.ncols m]
  return (r, c)

neighboursAndDanger :: Matrix Int -> (Int, Int) -> [(Int, (Int, Int))]
neighboursAndDanger m p = neighbours m p & fmap \t -> (m ! t, t)


type Scores = Map.Map (Int, Int) Node

updatePos :: (a -> a) -> (Int, Int) -> Matrix a -> Matrix a
updatePos f p m = m ! p & \a -> unsafeSet (f a) p m

dijkstra :: Matrix Int -> Scores -> Int -> (Int, Int) -> Scores
dijkstra md s distance p =
  foldr (\(k, v) -> Map.adjust (<> v) k) s (fmap (\t -> (t, md ! t + distance & Tentative)) (neighbours md p))
    & Map.adjust (<> Visited distance) p

visited :: Node -> Node
visited (Tentative i) = Visited i
visited _             = error "can only visit tentative nodes"

tentative :: Node -> Maybe Int
tentative (Tentative i) = Just i
tentative _             = Nothing

unvisited :: Scores -> [(Int, (Int, Int))]
unvisited m = (sort >>> take 1) do
  (p, t) <- Map.toList m
  t'     <- tentative t & maybeToList
  (t', p) & return


dijkstraStep :: Matrix Int -> Scores -> Scores
dijkstraStep md s = foldr (\(d, p) s' -> dijkstra md s' d p) s (unvisited s)

partOne :: IO ()
partOne = do
  md <- readInput
  let mn = positions md & fmap (, Unvisited) & Map.fromList & Map.insert (1, 1) (Tentative 0)
  print md
  print mn
  print (unvisited mn)
  let result = until (unvisited >>> length >>> traceShowId >>> (== 0)) (dijkstraStep md) mn
  print result

partTwo :: IO ()
partTwo = print "two"

main :: IO ()
main = do
  partOne
  partTwo
