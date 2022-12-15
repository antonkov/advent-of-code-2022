import Std.Data.HashMap
import Mathlib.Init.Data.List.Basic

open Std HashMap

def input := "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"

structure Pos where
  x: Nat
  y: Nat
deriving Repr, BEq, Hashable, Inhabited

def neighbours (p: Pos): List Pos :=
  let x := p.x
  let y := p.y
  [ {x := x + 1, y := y}, {x := x - 1, y := y}, {x := x, y := y + 1}, {x := x, y := y - 1} ]

def canGo (fromHeight toHeight : Char) : Bool := fromHeight.toNat >= toHeight.toNat - 1

def bfs (m: HashMap Pos Char) (starts: List Pos): HashMap Pos Nat := Id.run do
  let mut dist := HashMap.ofList (starts.map (·, 0))
  let mut q : Queue Pos := Queue.empty.enqueueAll starts
  while true do
    if let some (v, rest) := q.dequeue? then
      q := rest
      for u in neighbours v do
        if HashMap.contains m u
        && !HashMap.contains dist u
        && canGo (m.find! v) (m.find! u) then
          dist := dist.insert u (dist.find! v + 1)
          q := q.enqueue u
    else break

  dist

def buildMap (input: String): HashMap Pos Char := Id.run do
  let mut m : HashMap Pos Char := HashMap.empty
  for (y, line) in (input.splitOn "\n").mapWithIndex Prod.mk do
    for (x, c) in line.toList.mapWithIndex Prod.mk do
      m := m.insert {x := x, y := y} c
  m

def findChar (m: HashMap Pos Char) (c: Char): List Pos :=
  m.toList.filter (fun (_, v) => v = c) |>.map Prod.fst

def main : IO Unit := do
  let input <- IO.FS.readFile "day12.input"
  let mut geoMap := buildMap input
  let start := findChar geoMap 'S'
  geoMap := geoMap.insert start[0]! 'a'

  let starts := findChar geoMap 'a'
  let finish := findChar geoMap 'E'
  let dist := bfs (geoMap |>.insert finish[0]! 'z') starts
  IO.println $ dist.find! finish[0]!

#eval main
 
