module Day3

import System.File
import Data.String
import Data.List
import Data.List1
import Data.Nat

score : Char -> Integer 
score c = if isUpper c then
          (cast c) - (cast 'A') + 27 else
          (cast c) - (cast 'a') + 1

listScore : List Char -> Integer
listScore = sum . (map score) . (take 1)

solveOne : List Char -> Integer
solveOne s = listScore $ intersect (take l s) (drop l s)
  where
    l : Nat 
    l = divNatNZ (List.length s) 2 SIsNonZero

solve : List (List Char) -> Integer
solve = sum . (map solveOne)

solve2 : List (List Char) -> Integer
solve2 (x::y::z::xs) = (listScore $ intersect x (intersect y z)) + (solve2 xs)
solve2 _ = 0

partial main : IO ()
main = do
  Right content <- readFile "src/day3.test" | Left e => printLn e
  printLn . solve2 . (map unpack) . lines $ content