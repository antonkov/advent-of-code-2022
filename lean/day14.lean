import Lean.Data.HashSet

open Lean HashSet

def input := "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"

structure Pos where
  x : Nat
  y : Nat
deriving Repr, Inhabited, BEq, Hashable

def buildLines (input : String) : List (List Pos) := Id.run do
  let parseLine (line : String) : List Pos :=
    let parsePos (pos : String) : Pos :=
      if let [x, y] := pos.splitOn "," then
        Pos.mk x.toNat! y.toNat!
      else panic! "invalid pos"
    line.splitOn " -> " |>.map parsePos

  input.splitOn "\n" |>.map parseLine

def buildMap (lines : List (List Pos)) : HashSet Pos := Id.run do
  let mut map : HashSet Pos := HashSet.empty
  for line in lines do
    for (p₁, p₂) in line.zip (line.drop 1) do
      let x₁ := min p₁.x p₂.x
      let x₂ := max p₁.x p₂.x
      let y₁ := min p₁.y p₂.y
      let y₂ := max p₁.y p₂.y
      for x in [x₁:x₂ + 1] do
        for y in [y₁:y₂ + 1] do
          map := map.insert ⟨x, y⟩
  map

def sandStep (pos : Pos) : List Pos :=
  [⟨ pos.x, pos.y + 1 ⟩, ⟨ pos.x - 1, pos.y + 1 ⟩, ⟨ pos.x + 1, pos.y + 1⟩]

def dropSand (map : HashSet Pos) : Option Pos := Id.run do
  let mut p := Pos.mk 500 0
  while true do
    if p.y > 1000 then
      return none
    let mut found := false
    for q in sandStep p do
      if !map.contains q then
        p := q
        found := true
        break
    if !found then
      break
  if p.y == 0 then none else some p

def map := buildMap $ buildLines input

def dropCount (map : HashSet Pos) : Nat := Id.run do
  let mut map := map
  let mut result := 0
  while true do
    if let some p := dropSand map then
      map := map.insert p
      result := result + 1
    else
      break
  result

def solveTask (input : String) : Nat := Id.run do
  let mut lines := buildLines input
  let maxY := lines.join |>.map (·.y) |>.maximum?.get!
  lines := [⟨ 0, maxY + 2 ⟩, ⟨ 1000, maxY + 2 ⟩] :: lines
  let map := buildMap lines
  dropCount map + 1

#eval solveTask input

def main : IO Unit := do
  let input ← IO.FS.readFile "day14.input"
  IO.print s!"{solveTask input}"