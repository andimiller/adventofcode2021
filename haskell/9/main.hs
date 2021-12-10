{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text
import           Data.Char                      ( digitToInt )
import           Data.Function
import           Data.Functor                   ( (<&>) )
import           Data.List                     as List
import           Data.Matrix                   as Matrix
import           Data.Maybe                     ( catMaybes
                                                , isJust
                                                )
import           Data.Set                      as Set
import           Data.Text                      ( pack )

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

neighbours :: Matrix Int -> Int -> Int -> [Int]
neighbours m x y =
  catMaybes [Matrix.safeGet (x - 1) y m, Matrix.safeGet (x + 1) y m, Matrix.safeGet x (y - 1) m, Matrix.safeGet x (y + 1) m]


isLow :: Matrix Int -> Int -> Int -> Bool
isLow m x y = all (> unsafeGet x y m) (neighbours m x y)

positions :: Matrix Int -> [(Int, Int)]
positions m = do
  r <- [1 .. Matrix.nrows m]
  c <- [1 .. Matrix.ncols m]
  return (r, c)

partOne :: IO ()
partOne = do
  m <- readInput
  let lows = List.filter (uncurry (isLow m)) (positions m)
  let results = List.map ((+ 1) . flip (uncurry unsafeGet) m) lows
  let result  = sum results
  print result

getPos :: Matrix Int -> (Int, Int) -> Maybe Int
getPos = flip (uncurry safeGet)

unsafeGetPos :: Matrix Int -> (Int, Int) -> Int
unsafeGetPos = flip (uncurry unsafeGet)

neighboursPos :: Matrix Int -> (Int, Int) -> [(Int, Int)]
neighboursPos m (x, y) = List.filter (getPos m >>> isJust) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]


baisinMembers :: Matrix Int -> (Int, Int) -> Set (Int, Int)
baisinMembers m (x, y) =
  List.filter (unsafeGetPos m >>> (> unsafeGet x y m)) (neighboursPos m (x, y)) & List.filter (unsafeGetPos m >>> (< 9)) & Set.fromList

expandBaisin :: Matrix Int -> Set (Int, Int) -> Set (Int, Int)
expandBaisin m ps = Set.map (baisinMembers m) ps & Set.unions & Set.union ps

repeatUntilStable :: Eq a => (a -> a) -> a -> a
repeatUntilStable f prev | prev == f prev = prev
                         | otherwise      = repeatUntilStable f (f prev)

partTwo :: IO ()
partTwo = do
  m <- readInput
  let lows = List.filter (uncurry (isLow m)) (positions m) <&> Set.singleton
  let baisins      = List.map (repeatUntilStable (expandBaisin m)) lows
  let baisinSizes  = List.map Set.size baisins
  let threeLargest = List.take 3 (List.reverse (List.sort baisinSizes))
  let result       = product threeLargest
  print result

main :: IO ()
main = do
  partOne
  partTwo
