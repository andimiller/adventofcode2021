{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text           ( sepBy1
                                                , parseOnly
                                                , many1
                                                , char
                                                , Parser
                                                , digit
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
                                                )
import           Data.Text                      ( pack )
import           Data.Maybe                     ( maybe
                                                , catMaybes
                                                , fromMaybe
                                                , isNothing
                                                )
import           Data.Monoid                    ( Endo(..)
                                                , mconcat
                                                , appEndo
                                                )
import           Data.List                      ( unfoldr )
import           Control.Monad.Loops            ( unfoldM )

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


neighbours :: Matrix a -> (Int, Int) -> [(Int, Int)]
neighbours m (x, y) =
  do
      xc <- [x - 1 .. x + 1]
      yc <- [y - 1 .. y + 1]
      return (Matrix.safeGet xc yc m $> (xc, yc))
    & catMaybes
    & filter (/= (x, y))

mapPosition :: (a -> a) -> (Int, Int) -> Matrix a -> Matrix a
mapPosition f (x, y) m = Matrix.setElem (f (Matrix.unsafeGet x y m)) (x, y) m

mapNeighbours :: (a -> a) -> (Int, Int) -> Matrix a -> Matrix a
mapNeighbours f p m = map (Endo . mapPosition f) (neighbours m p) & mconcat & flip appEndo m

flash :: Num n => (Int, Int) -> Matrix (Maybe n) -> Matrix (Maybe n)
flash p m = mapPosition (const Nothing) p m & mapNeighbours (fmap (+ 1)) p

positions :: Matrix a -> [(Int, Int)]
positions m = do
  r <- [1 .. Matrix.nrows m]
  c <- [1 .. Matrix.ncols m]
  return (r, c)

getFlashTargets :: (Ord n, Num n) => Matrix (Maybe n) -> [(Int, Int)]
getFlashTargets m = filter (flip (uncurry unsafeGet) m >>> maybe False (> 9)) (positions m)


flashTargets :: (Ord n, Num n) => Matrix (Maybe n) -> Matrix (Maybe n)
flashTargets m = map (flash >>> Endo) (getFlashTargets m) & mconcat & flip appEndo m


repeatUntilStable :: Eq a => (a -> a) -> a -> a
repeatUntilStable f prev | prev == f prev = prev
                         | otherwise      = repeatUntilStable f (f prev)

flashCounter :: Maybe n -> Int
flashCounter (Just _) = 0
flashCounter Nothing  = 1

flashstep :: (Ord n, Num n) => (Int, Matrix n) -> (Int, Matrix n)
flashstep (total, m) =
  fmap (Just . (+ 1)) m & repeatUntilStable flashTargets & \m -> (fmap flashCounter m & sum & (+ total), fmap (fromMaybe 0) m)

partOne :: IO ()
partOne = do
  m <- readInput
  let run               = replicate 100 (Endo flashstep) & mconcat
  let (flashes, result) = appEndo run (0, m)
  print flashes

-- this version of flashstep returns nothing when they all flash
flashstep2 :: (Ord n, Num n) => Matrix n -> Maybe ((), Matrix n)
flashstep2 =
  fmap (Just . (+ 1)) >>> repeatUntilStable flashTargets >>> \m -> if all isNothing m then Nothing else Just ((), fmap (fromMaybe 0) m)


partTwo :: IO ()
partTwo = do
  m <- readInput
  let result = unfoldr flashstep2 m & length & (+ 1)
  print result

main :: IO ()
main = do
  partOne
  partTwo
