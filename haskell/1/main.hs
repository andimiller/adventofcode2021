import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import           Text.Read

readInput :: IO [Integer]
readInput = readFile "input.txt" <&> lines <&> map read

sliding :: Int -> [a] -> [[a]]
sliding n [] = []
sliding n xs = take n xs : sliding n (tail xs) & filter ((n ==) . length)

lessThan :: Ord (a) => [a] -> Bool
lessThan [a, b] = a < b

partOne :: IO ()
partOne =
  readInput <&> sliding 2 <&> filter lessThan <&> length <&> show >>= putStrLn
partTwo :: IO ()
partTwo =
  readInput
    <&> sliding 3
    <&> map sum
    <&> sliding 2
    <&> filter lessThan
    <&> length
    <&> show
    >>= putStrLn


main :: IO ()
main = do
  partOne
  partTwo
