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


def getHeight (rocks : HashSet Pos) := rocks.toList.map (·.y) |>.maximum?.getD 0

def move (dir : Char) (figure : List Pos) : List Pos :=
  match dir with
  | '>' => figure.map (fun p => {p with x := p.x + 1} )
  | '<' => figure.map (fun p => {p with x := p.x - 1} )
  | 'v' => figure.map (fun p => {p with y := p.y - 1} )
  | _ => figure

def intersects (rocks : HashSet Pos) (figure : List Pos) : Bool :=
  figure.any (fun p => rocks.contains p || p.x < 0 || p.x >= 7 || p.y <= 0)

def solveTask (input : List Char) (numStages : Nat) : IO Int := do
  let mut rocks : HashSet Pos := HashSet.empty
  let mut iter := 0
  for stage in [:numStages] do
    let mut figure := spawn stage (getHeight rocks + 4)
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
  pure $ getHeight rocks

-- #eval solveTask 2022 

def main : IO Unit := do 
  let input ← IO.FS.readFile "day17.input"
  let res ← solveTask (input.toList) 2022
  IO.println res

#eval main