module Day1

import System.File
import Data.String
import Data.List
import Data.List1

maxList : List1 Integer -> Integer
maxList (x:::xs) = foldr max x xs

sums : List String -> List1 Integer
sums xs = map (sum . (map cast)) $ splitOn "" xs

solve : List String -> Integer
solve = maxList . sums

solve2 : List String -> Integer
solve2 = sum . (List.take 3) . reverse . sort . toList . sums

main : IO ()
main = do
  Right content <- readFile "src/test.md" | Left e => printLn e
  printLn . solve . lines $ content 