import Lean.Data.Parsec
import Mathlib.Data.List.Defs

open Lean Parsec

def input := "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"

structure Pos where
  x : Int
  y : Int
deriving Repr

structure Sensor where
  pos : Pos
  closestBeacon : Pos
deriving Repr

def parseInt : Parsec Int := do
  let s ← manyChars (digit <|> pchar '-')
  pure s.toInt!

def parseSensor : Parsec Sensor := do
  let _ ← pstring "Sensor at x="
  let x ← parseInt
  let _ ← pstring ", y="
  let y ← parseInt
  let _ ← pstring ": closest beacon is at x="
  let x₂ ← parseInt
  let _ ← pstring ", y="
  let y₂  ← parseInt
  pure { pos := { x := x, y := y }, closestBeacon := { x := x₂, y := y₂ } }

def solveForRow (sensors : List Sensor) (y: Int) (maxCoord : Int) := Id.run do
  let segments := sensors.filterMap
    (fun s =>
      let dist : Int := (s.pos.x - s.closestBeacon.x).natAbs + (s.pos.y - s.closestBeacon.y).natAbs
      let norm : Int := (s.pos.y - (y : Int)).natAbs 
      if norm <= dist then some ( max (s.pos.x - (dist - norm)) 0 , min (s.pos.x + (dist - norm)) maxCoord )
      else none
    )
  -- let bcs := sensors.map (fun s => s.closestBeacon)
  --                 |>.filter (fun s => s.y == y)
  --                 |>.map (fun s => s.x)
  --                 |>.dedup

  let pts := segments.toArray.concatMap (fun x => #[(x.1, 0), (x.2, 1)])
              |>.qsort (fun x y => if x.1 != y.1 then x.1 < y.1 else x.2 < y.2)
  let mut cnt := 0
  let mut lst := -1 
  for p in pts do
    if p.2 == 0 then
      if cnt == 0 && p.1 - lst > 1 then
        return some (lst + 1)
      cnt := cnt + 1
    else
      cnt := cnt - 1
      if cnt == 0 then
        lst := p.1
  if lst < maxCoord then some (lst + 1) else none

def findRow (input : String) (maxCoord : Nat) := Id.run do
  let sensors := input.splitOn "\n" |>.map parseSensor.run |>.filterMap (fun x => x.toOption)
  for y in [0:maxCoord] do
    if let some x := solveForRow sensors y maxCoord then
      return (x, y)
    else
      continue
  panic! "No solution found"

#eval findRow input 20

def main : IO Unit := do
  let input ← IO.FS.readFile "day15.input"
  let result := findRow input 4000000
  IO.println result

def result := (3131431, 2647448)
#eval result.1 * 4000000 + result.2
-- Row is 2647448
