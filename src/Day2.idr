module Day2

import System.File
import Data.String
import Data.List
import Data.List1

data RPS = Rock | Paper | Scissors
data XYZ = X | Y | Z

partial parseRPS : String -> RPS
parseRPS "A" = Rock
parseRPS "B" = Paper
parseRPS "C" = Scissors 

partial parseXYZ : String -> XYZ 
parseXYZ "X" = X
parseXYZ "Y" = Y
parseXYZ "Z" = Z

asRPS : XYZ -> RPS
asRPS X = Rock
asRPS Y = Paper
asRPS Z = Scissors

Eq RPS where
  (==) Rock Rock = True
  (==) Paper Paper = True
  (==) Scissors Scissors = True
  (==) _ _ = False

winPoints : RPS -> RPS -> Integer
winPoints Paper Scissors = 6
winPoints Rock Paper = 6
winPoints Scissors Rock = 6
winPoints x y = if x == y then 3 else 0

throwPoints : RPS -> Integer
throwPoints Rock = 1
throwPoints Paper = 2
throwPoints Scissors = 3

points : (RPS, XYZ) -> Integer
points (opp, you) = winPoints opp (asRPS you) + throwPoints (asRPS you)

shouldThrow : (RPS, XYZ) -> RPS
shouldThrow (Rock, X) = Scissors
shouldThrow (Paper, X) = Rock
shouldThrow (Scissors, X) = Paper
shouldThrow (x, Y) = x
shouldThrow (Rock, Z) = Paper
shouldThrow (Paper, Z) = Scissors
shouldThrow (Scissors, Z) = Rock

points2 : (RPS, XYZ) -> Integer
points2 (opp, outcome) = let you = shouldThrow (opp, outcome) in
  winPoints opp you + throwPoints you 

partial parse : String -> Maybe (RPS, XYZ)
parse s = case words s of
  (x :: y :: Nil) => Just (parseRPS x, parseXYZ y)
  _ => Nothing

partial solve : List String -> Integer
solve = sum . (map points) . (mapMaybe parse)

partial solve2 : List String -> Integer
solve2 = sum . (map points2) . (mapMaybe parse)

partial main : IO ()
main = do
  Right content <- readFile "src/day2.test" | Left e => printLn e
  printLn . solve2 . lines $ content