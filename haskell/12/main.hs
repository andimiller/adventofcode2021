{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Attoparsec.Text           ( sepBy1
                                                , parseOnly
                                                , many1
                                                , char
                                                , Parser
                                                , inClass
                                                , satisfy
                                                )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import           Data.Text                      ( pack
                                                , Text
                                                )
import           Control.Applicative            ( (<|>) )
import           Data.List                      ( nub
                                                , sort
                                                , group
                                                )
import           Data.List.NonEmpty            as NonEmpty
                                                ( NonEmpty(..)
                                                , (<|)
                                                )
import qualified Data.List.NonEmpty            as NonEmpty
                                                ( filter )

-- functions are composed left to right
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f

data Cave = Big Text | Small Text deriving (Show, Eq, Ord)

big :: Parser Cave
big = inClass "A-Z" & satisfy & many1 & fmap (pack >>> Big)

small :: Parser Cave
small = inClass "a-z" & satisfy & many1 & fmap (pack >>> Small)

cave :: Parser Cave
cave = big <|> small

data Link = Link { from ::Cave, to :: Cave } deriving (Show, Eq)

reverseLink :: Link -> Link
reverseLink (Link a b) = Link b a

link :: Parser Link
link = do
  a <- cave
  char '-' & void
  b <- cave
  return (Link a b)

input :: Parser [Link]
input = link `sepBy1` char '\n'

-- helper to discard eithers
rethrow :: (Show l) => Either l r -> r
rethrow = either (show >>> error) id

-- read the input file
readInput :: IO [Link]
readInput = readFile ("input.txt" :: String) <&> pack >>> parseOnly input >>> rethrow

type Route = NonEmpty Cave

canTake :: Route -> Link -> Bool
canTake (here :| _) (Link f (Big _)) | here == f = True
                                     | otherwise = False
canTake route@(here :| _) (Link f t@(Small _)) | here == f && notElem t route = True
                                               | otherwise                    = False

branch :: [Link] -> Route -> [Route]
branch _  r@(Small "end" :| _) = [r]
branch ls r                    = map ((<| r) . to) (filter (canTake r) ls)

repeatUntilStable :: Eq a => (a -> a) -> a -> a
repeatUntilStable f prev | prev == f prev = prev
                         | otherwise      = repeatUntilStable f (f prev)

step :: [Link] -> [Route] -> [Route]
step ls = concatMap (branch ls)

nonEmptySingleton :: a -> NonEmpty a
nonEmptySingleton = (:| [])

partOne :: IO ()
partOne = do
  i <- readInput
  let backlinks = map reverseLink i
  let links     = i ++ backlinks & nub
  let routes = repeatUntilStable (step links) [nonEmptySingleton (Small "start")]
  print (length routes)

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall (Big   _) = False

canDoubleVisitSmall :: Cave -> Route -> Bool
canDoubleVisitSmall (Small "start") _ = False
canDoubleVisitSmall _               r = 2 `notElem` (NonEmpty.filter isSmall r & sort & group & map length)


-- this version allows for double visiting of one small cave per run
canTake2 :: Route -> Link -> Bool
canTake2 (here :| _) (Link f (Big _)) | here == f = True
                                      | otherwise = False
canTake2 route@(here :| _) (Link f t@(Small _)) | here == f && (notElem t route || canDoubleVisitSmall t route) = True
                                                | otherwise = False

branch2 :: [Link] -> Route -> [Route]
branch2 _  r@(Small "end" :| _) = [r]
branch2 ls r                    = map ((<| r) . to) (filter (canTake2 r) ls)

step2 :: [Link] -> [Route] -> [Route]
step2 ls = concatMap (branch2 ls)

partTwo :: IO ()
partTwo = do
  i <- readInput
  let backlinks = map reverseLink i
  let links     = i ++ backlinks & nub
  let routes = repeatUntilStable (step2 links) [nonEmptySingleton (Small "start")]
  print (length routes)

main :: IO ()
main = do
  partOne
  partTwo
