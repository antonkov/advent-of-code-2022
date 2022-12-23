import Lean.Data.HashMap

open Lean

def input := "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"

structure Pos where
  row : Nat 
  col : Nat 
deriving BEq, Hashable, Repr, Inhabited

inductive Tile where
  | wall
  | space
deriving Repr

inductive Move where
  | left
  | right
  | forward (n : String)
deriving Repr

def Map := HashMap Pos Tile

partial def parseMoves (s : List Char) : List Move :=
  match s with
  | [] => []
  | 'L' :: rest => Move.left :: parseMoves rest
  | 'R' :: rest => Move.right :: parseMoves rest
  | s =>
    let (n, rest) := s.span Char.isDigit
    Move.forward (n.map Char.toString |>.foldl (fun a b => a ++ b) "") :: parseMoves rest

def parseInput (s : String) : Map × (List Move) :=
  let parseMap (s : String) : Map := Id.run do
    let mut map := HashMap.empty
    let mut row := 1
    for line in s.splitOn "\n" do
      let mut col := 1
      for c in line.toList do
        match c with
        | '#' => map := map.insert {row, col} Tile.wall
        | '.' => map := map.insert {row, col} Tile.space
        | _ => ()
        col := col + 1
      row := row + 1
    map

  if let [map, moves] := s.splitOn "\n\n" then
    (parseMap map, parseMoves moves.toList)
  else
    (HashMap.empty, [])

def maxCoord := 210
def wrap (n : Nat) := if n > maxCoord then 1 else if n < 1 then maxCoord else n

def solveTask (map : Map) (moves : List Move) := Id.run do
  let nextInDir (dir : Nat) (startPos : Pos) : Pos := Id.run do
    let mut pos := startPos 
    for _ in [:maxCoord] do
      match dir with
      | 0 => pos := {pos with col := wrap (pos.col + 1) }
      | 1 => pos := {pos with row := wrap (pos.row + 1) }
      | 2 => pos := {pos with col := wrap (pos.col - 1) }
      | 3 => pos := {pos with row := wrap (pos.row - 1) }
      | _ => panic! "invalid dir"
      if map.find? pos |>.isSome then
        break
      else
        continue
    pos

  let mut dir := 0
  let mut pos := nextInDir 0 {row := 1, col := 1}
  for move in moves do
    match move with
    | Move.left => dir := (dir + 3) % 4
    | Move.right => dir := (dir + 1) % 4
    | Move.forward n =>
      let n := n.toNat!
      for _ in [:n] do
        let nextPos := nextInDir dir pos
        match map.find? nextPos with
        | some Tile.wall => break
        | _ => pos := nextPos
  
  1000 * pos.row + 4 * pos.col + dir

def main : IO Unit := do
--  let input ← IO.FS.readFile "day22.input"
  let parsed := parseInput input
  IO.println $ solveTask parsed.1 parsed.2

#eval main