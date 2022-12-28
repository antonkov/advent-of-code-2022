import Lean.Data.HashSet

open Lean

def input := ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>".toList

#eval input.length

structure Pos where
  x : Int 
  y : Int 
deriving BEq, Hashable

def spawn (type : Nat) (row : Int) : List Pos :=
  match type % 5 with
  | 0 => [⟨ 2, row ⟩, ⟨ 3, row ⟩, ⟨ 4, row ⟩, ⟨ 5, row ⟩]
  | 1 => [⟨ 3, row ⟩, ⟨ 2, row + 1 ⟩, ⟨ 3, row + 1 ⟩, ⟨ 4, row + 1 ⟩, ⟨ 3, row + 2 ⟩]
  | 2 => [⟨ 2, row ⟩, ⟨ 3, row ⟩, ⟨ 4, row ⟩, ⟨ 4, row + 1 ⟩, ⟨ 4, row + 2 ⟩]
  | 3 => [⟨ 2, row ⟩, ⟨ 2, row + 1 ⟩, ⟨ 2, row + 2 ⟩, ⟨ 2, row + 3 ⟩ ]
  | 4 => [⟨ 2, row ⟩, ⟨ 3, row ⟩, ⟨ 2, row + 1 ⟩, ⟨ 3, row + 1 ⟩ ]
  | _ => []

def List.sort (xs : List String) : List String :=
  xs.toArray.qsort (· < ·) |>.toList

def getHeight (rocks : List Pos) := rocks.map (·.y) |>.maximum?.getD 0

def move (dir : Char) (figure : List Pos) : List Pos :=
  match dir with
  | '>' => figure.map (fun p => {p with x := p.x + 1} )
  | '<' => figure.map (fun p => {p with x := p.x - 1} )
  | 'v' => figure.map (fun p => {p with y := p.y - 1} )
  | _ => figure

def intersects (rocks : HashSet Pos) (figure : List Pos) : Bool :=
  figure.any (fun p => rocks.contains p || p.x < 0 || p.x >= 7 || p.y <= 0)

def solveTask (input : List Char) (numStages : Nat) : IO (Array Nat) := do
  let mut rocks : HashSet Pos := HashSet.empty
  let mut iter := 0
  let mut height : Nat := 0
  let mut hs := #[]
  for stage in [:numStages] do
    let mut figure := spawn stage (height + 4)
    while true do
      let dir := input[iter % input.length]!
      iter := iter + 1
      let moved := move dir figure
      if !intersects rocks moved then
        figure := moved
      let movedDown := move 'v' figure
      if intersects rocks movedDown then
        break
      else
        figure := movedDown
    rocks := rocks.insertMany figure
    let newHeight := height.max (getHeight figure).toNat
    hs := hs.push (newHeight - height)
    height := newHeight
    if stage % 1000 == 0 then
      IO.println s!"Stage: {stage/1000}"
  /-let mut maxDepth := 0
  for i in [:7] do
    let curDepth : Nat := rocks.toList.filter (·.x.toNat == i) |>.map (·.y.toNat) |>.maximum?.getD 0
    maxDepth := maxDepth.max (height - curDepth).toNat
  IO.println s!"Max depth: {maxDepth}"-/
    /-
  for i in [:10] do
    let xs := rocks.toList.filter (·.y == height - i) |>.map (fun p => p.x.toNat)
    let res := List.range 7 |>.map (fun x => if xs.contains x then '#' else '.')
    IO.println res-/
  pure $ hs 

-- #eval solveTask 2022 

-- Cycle is 1725
def findCycle (xs : Array Nat) : Nat := Id.run do
  let n := xs.size
  for len in [2:n] do
    let mut found := true
    for i in [n / 2 : n - len] do
      if xs[i]! != xs[i + len]! then
        found := false
        break
    if found then
      return len
  0


def main : IO Unit := do 
  let input ← IO.FS.readFile "day17.input"
  let numIters := 1000000000000
  let n := 10000
  let xs ← solveTask (input.toList) n
  let length := findCycle xs
  let numCycles := (numIters - n / 2) / length
  let start := numIters - numCycles * length
  let mut sumCycle := 0
  for i in [start : start + length] do
    sumCycle := sumCycle + xs[i]!
  let mut sumPrefix := 0
  for i in [:start] do
    sumPrefix := sumPrefix + xs[i]!
  let sum := sumPrefix + sumCycle * numCycles
  IO.println s!"{sum}"

#eval main