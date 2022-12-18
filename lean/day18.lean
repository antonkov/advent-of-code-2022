import Lean.Data.HashSet

open Lean
open Std

def input := "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"

structure Point where
  x : Int
  y : Int
  z : Int
deriving BEq, Hashable

def parsePoint (s : String) : Option Point :=
  if let [x, y, z] := s.splitOn "," |>.map String.toNat! then
    Point.mk x y z
  else
    none

def parseInput (s : String) : List Point := s.splitOn "\n" |>.map parsePoint |>.filterMap id

def neighbours (p : Point) : List Point :=
  [Point.mk (p.x - 1) p.y p.z, Point.mk (p.x + 1) p.y p.z,
   Point.mk p.x (p.y - 1) p.z, Point.mk p.x (p.y + 1) p.z,
   Point.mk p.x p.y (p.z - 1), Point.mk p.x p.y (p.z + 1)]

def maxCoord := 20

def solveTask (input : String) : IO Nat := do
  let pointsList := parseInput input
  let points : HashSet Point := HashSet.empty.insertMany pointsList

  let startOutside := Point.mk maxCoord maxCoord maxCoord
  let mut outside : HashSet Point := HashSet.empty.insert startOutside
  let mut q : Queue Point := Queue.empty.enqueue startOutside
  while true do
    if let some (v, rest) := q.dequeue? then
      q := rest
      for n in neighbours v do
        let inBounds (x : Int) := x >= -1 && x <= maxCoord
        if inBounds n.x && inBounds n.y && inBounds n.z && !outside.contains n && !points.contains n then
          outside := outside.insert n
          q := q.enqueue n
    else
      break
  let mut area := 0
  for p in points do
    for n in neighbours p do
      if outside.contains n then
        area := area + 1
  pure area

#eval solveTask input

def main : IO Unit := do
  let input ← IO.FS.readFile "day18.input"
  let res ← solveTask input
  IO.println res

#eval main