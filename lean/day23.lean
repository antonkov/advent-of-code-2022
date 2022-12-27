import Lean.Data.HashSet
import Lean.Data.HashMap

open Lean

def input := "..............
..............
.......#......
.....###.#....
...#...#.#....
....#...##....
...#.###......
...##.#.##....
....#..#......
..............
..............
.............."

/-def input := ".....
..##.
..#..
.....
..##.
....."-/

structure Pos where
  x : Int
  y : Int
deriving BEq, Hashable, Repr

def parseInput (s : String) : HashSet Pos := Id.run do
  let mut set := HashSet.empty
  let lines := s.splitOn "\n"
  for i in [:lines.length] do
    let line := lines[i]!.toList
    for j in [:line.length] do
      if line[j]! == '#' then
        set := set.insert { x := j, y := i }
  set

def draw (set : HashSet Pos) : IO Unit := do
  let intMin : Int := -10000
  let intMax : Int := 10000
  let mut minX := intMax
  let mut maxX := intMin
  let mut minY := intMax
  let mut maxY := intMin
  let mut countEmpty := 0
  for p in set do
    minX := min minX p.x
    maxX := max maxX p.x
    minY := min minY p.y
    maxY := max maxY p.y
  for i in [:(maxY - minY + 1).toNat] do
    let mut s := ""
    for j in [:(maxX - minX + 1).toNat] do
      let p := { x := j + minX, y := i + minY }
      if set.contains p then
        s := s ++ "#"
      else
        s := s ++ "."
        countEmpty := countEmpty + 1
    IO.println s
  IO.println countEmpty

structure Direction where
  dir : Pos
  adj : List Pos

def dirs : List Direction := [
  { dir := { x := 0, y := -1 }, adj := [{ x := -1, y := -1 }, { x := 0, y := -1}, { x := 1, y := -1}] },
  { dir := { x := 0, y := 1 }, adj := [{ x := -1, y := 1 }, { x := 0, y := 1}, { x := 1, y := 1}] },
  { dir := { x := -1, y := 0 }, adj := [{ x := -1, y := -1 }, { x := -1, y := 0}, { x := -1, y := 1}] },
  { dir := { x := 1, y := 0 }, adj := [{ x := 1, y := -1 }, { x := 1, y := 0}, { x := 1, y := 1}] }
]

-- all 8 neighbours
def neighbours (p : Pos) : List Pos := [
  { x := p.x - 1, y := p.y - 1 },
  { x := p.x - 1, y := p.y },
  { x := p.x - 1, y := p.y + 1 },
  { x := p.x, y := p.y - 1 },
  { x := p.x, y := p.y + 1 },
  { x := p.x + 1, y := p.y - 1 },
  { x := p.x + 1, y := p.y },
  { x := p.x + 1, y := p.y + 1 }
]

instance : HAdd Pos Pos Pos where
  hAdd p1 p2 := { x := p1.x + p2.x, y := p1.y + p2.y }

def doRound (pos : HashSet Pos) (dirs: List Direction) : (HashSet Pos) × Nat := Id.run do
  let mut newPos := HashSet.empty
  -- from pos to who wants to move into this pos.
  let mut intent : HashMap Pos (List Pos) := HashMap.empty
  for p in pos do
    let ns := neighbours p
    if ns.any (fun n => pos.contains n) then
      let mut found := false
      for ⟨ dir, adj ⟩ in dirs do
        if adj.all (fun a => !pos.contains (p + a)) then
          intent := intent.insert (p + dir) (p :: intent.findD (p + dir) [])
          found := true
          break
      if !found then
        newPos := newPos.insert p
    else
      newPos := newPos.insert p

  let mut numMoved := 0
  for ⟨ p, ps ⟩ in intent.toList do
    if ps.length == 1 then
      newPos := newPos.insert p
      numMoved := numMoved + 1
    else
      for p in ps do
        newPos := newPos.insert p
  (newPos, numMoved)

partial def doRounds (dirs : List Direction) (pos : HashSet Pos) : IO Nat := do
  let mut numRound := 0
  let mut pos := pos
  while true do
    let (newPos, numMoved) := doRound pos (dirs.rotateLeft numRound)
    if numMoved == 0 then
      return numRound + 1
    else
      pos := newPos
      numRound := numRound + 1
      if numRound % 100 == 0 then
        IO.println numRound
  pure 0

def main : IO Unit := do
  let input ← IO.FS.readFile "day23.input"
  let result ← doRounds dirs (parseInput input)
  IO.println s!"{result}"