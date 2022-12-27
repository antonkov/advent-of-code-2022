import Lean.Data.HashSet
import Lean.Data.HashMap

open Lean
open Std

def input := "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"

structure Pos where
  i : Int 
  j : Int
deriving BEq, Hashable, Repr 

instance : Add Pos where
  add p q := ⟨p.i + q.i, p.j + q.j⟩

structure Blizzard where
  pos : Pos
  dir : Nat

structure PosTime where
  pos : Pos
  time : Nat
deriving BEq, Hashable, Repr

def Nat.lcm (n m : Nat) := n * m / Nat.gcd n m

def getDir : Char → Pos := fun
  | '>' => ⟨0, 1⟩
  | '<' => ⟨0, -1⟩
  | '^' => ⟨-1, 0⟩
  | 'v' => ⟨1, 0⟩
  | _ => ⟨0, 0⟩

def wrap (x n : Int) := (x - 1 + n - 2) % (n - 2) + 1

def parseInput (input : String) : (HashSet PosTime) × (Nat × Nat × Nat)  := Id.run do
  let lines := input.splitOn "\n"
  let n := lines.length
  let m := lines.head!.length
  let modTime := Nat.lcm (n - 2) (m - 2)
  let mut maze := HashSet.empty
  for i in [:n] do
    let line := lines[i]!.toList
    for j in [:m] do
      let c := line[j]!
      if c == '.' then
        continue
      let dir := getDir c
      let mut pos : Pos := ⟨i, j⟩
      for t in [:modTime] do
        maze := maze.insert (⟨pos, t⟩ : PosTime)
        if c != '#' then
          pos := ⟨ wrap (pos.i + dir.i) n, wrap (pos.j + dir.j) m⟩

  (maze, (n, m, modTime))

#eval parseInput input |>.1.toList.filter (fun pt => pt.time == 1 && pt.pos.i == 2)
#eval 600*25*120

def neighbours (p : Pos) : List Pos := [
  { p with i := p.i + 1},
  { p with i := p.i - 1},
  { p with j := p.j + 1},
  { p with j := p.j - 1},
  p
]

def solveTask (input : String) : IO Unit := do
  let (maze, (n, m, modTime)) := parseInput input
  let inBounds (p : Pos) := p.i >= 0 && p.i < n && p.j >= 0 && p.j < m
  -- 1) To reach the goal it takes 305 minutes
  -- 2) Go back to start it takes 284 minutes
  -- 3) Then reach the goal again 316
  let startPos : Pos := ⟨ 0, 1 ⟩
  let startTime := 305 + 284
  let endPos : Pos := ⟨ n - 1, m - 2 ⟩
  let mut q := Queue.empty.enqueue (⟨startPos, startTime⟩ : PosTime)
  let mut dist : HashMap PosTime Nat := HashMap.empty.insert (⟨startPos, startTime⟩ : PosTime) 0
  let mut iter := 0
  while true do
    iter := iter + 1
    if let some (⟨pos, time⟩, q') := q.dequeue? then
      q := q'
      for nb in neighbours pos do
        let pt := ⟨nb, (time + 1) % modTime⟩
        if inBounds nb && !dist.contains pt && !maze.contains pt then
          q := q.enqueue pt
          let d := dist.find! ⟨pos, time⟩
          dist := dist.insert pt (d + 1)
          if nb.i == endPos.i && nb.j == endPos.j then
            IO.println (d + 1)
            return
    else
      break

  IO.println "Done"

def main : IO Unit := do
  let input ← IO.FS.readFile "day24.input"
  solveTask input

#eval 305 + 284 + 316
-- #eval main
-- #eval solveTask input

