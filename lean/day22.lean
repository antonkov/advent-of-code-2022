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

instance : ToString Move :=
  ⟨fun m => match m with
  | Move.left => "L"
  | Move.right => "R"
  | Move.forward n => "F" ++ n⟩

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

inductive Face := 
  | a | b | c | d | e | f
deriving Inhabited, Repr, BEq, Hashable

inductive Side where
  | up | down | left | right
deriving Inhabited, BEq

structure Edge where
  fromEdge : Face × Side
  toEdge : Face × Side
  inverse : Bool

def Vector := Int × Int
/-
  .ab
  .c.
  de.
  f..
-/
def toPos (face : Face) (dir : Nat) (scale : Nat) : Pos × Vector := 
 /- let p₀ : Pos := match face with
  | .a => {row := 0, col := 2}
  | .b => {row := 1, col := 0}
  | .c => {row := 1, col := 1}
  | .e => {row := 2, col := 2}
  | .d => {row := 1, col := 2}
  | .f => {row := 2, col := 3}--/

  let p₀ : Pos := match face with
  | .a => {row := 0, col := 1}
  | .b => {row := 0, col := 2}
  | .c => {row := 1, col := 1}
  | .d => {row := 2, col := 0}
  | .e => {row := 2, col := 1}
  | .f => {row := 3, col := 0}

  match dir with
  | 0 => ({row := p₀.row * scale + 1, col := p₀.col * scale + scale}, ⟨ 1, 0 ⟩)
  | 1 => ({row := p₀.row * scale + scale, col := p₀.col * scale + 1}, ⟨ 0, 1 ⟩)
  | 2 => ({row := p₀.row * scale + 1, col := p₀.col * scale + 1}, ⟨ 1, 0 ⟩)
  | 3 => ({row := p₀.row * scale + 1, col := p₀.col * scale + 1}, ⟨ 0, 1 ⟩)
  | _ => (default, (default, default))

structure Passage :=
  fromPosDir : Pos × Nat
  toPosDir : Pos × Nat
deriving Repr

def addPassage (fromPosDir toPosDir : Face × Nat) (scale : Nat) (inv : Bool) : List Passage := Id.run do
  let mut (fromPos, vFrom) := toPos fromPosDir.1 fromPosDir.2 scale
  let (toPos, vTo) := toPos toPosDir.1 toPosDir.2 scale

  if inv then
    (fromPos, vFrom) := (⟨ (fromPos.row + (scale - 1) * vFrom.1).toNat,
                           (fromPos.col + (scale - 1) * vFrom.2).toNat ⟩,
                           ⟨ -vFrom.1, -vFrom.2 ⟩)

  List.range scale |>.map (fun k =>
    let pos₀ := ⟨ (fromPos.row + vFrom.1 * k).toNat, (fromPos.col + vFrom.2 * k).toNat ⟩
    let pos₁ := ⟨ (toPos.row + vTo.1 * k).toNat, (toPos.col + vTo.2 * k).toNat ⟩
    [⟨ (pos₀, fromPosDir.2), pos₁, (toPosDir.2 + 2) % 4 ⟩,
     ⟨ (pos₁, toPosDir.2), pos₀, (fromPosDir.2 + 2) % 4 ⟩
    ]) |>.join


/-
  ..a.
  bcd.
  ..ef
-/
def edges : List Edge :=
  [
    {fromEdge := (.a, .up), toEdge := (.b, .up), inverse := true},
    {fromEdge := (.a, .left), toEdge := (.c, .up), inverse := false},
    {fromEdge := (.a, .right), toEdge := (.f, .right), inverse := true},
    {fromEdge := (.b, .left), toEdge := (.f, .down), inverse := true},
    {fromEdge := (.b, .down), toEdge := (.e, .down), inverse := true},
    {fromEdge := (.c, .down), toEdge := (.e, .left), inverse := true},
    {fromEdge := (.d, .right), toEdge := (.f, .up), inverse := true}
  ]

def Side.toDir : Side → Nat := 
  fun
  | .right => 0
  | .down => 1
  | .left => 2
  | .up => 3


def buildPassages (edges : List Edge) (scale : Nat) : List Passage :=
  edges.map (fun e =>
    let (fromFace, fromSide) := e.fromEdge
    let (toFace, toSide) := e.toEdge
    let fromPosDir := (fromFace, fromSide.toDir)
    let toPosDir := (toFace, toSide.toDir)
    addPassage fromPosDir toPosDir scale e.inverse) |>.join


/-
  .ab
  .c.
  de.
  f..
-/
def edges2 : List Edge :=
  [
    {fromEdge := (.a, .up), toEdge := (.f, .left), inverse := false},
    {fromEdge := (.a, .left), toEdge := (.d, .left), inverse := true},
    {fromEdge := (.b, .down), toEdge := (.c, .right), inverse := false},
    {fromEdge := (.b, .right), toEdge := (.e, .right), inverse := true},
    {fromEdge := (.b, .up), toEdge := (.f, .down), inverse := false},
    {fromEdge := (.c, .left), toEdge := (.d, .up), inverse := false},
    {fromEdge := (.e, .down), toEdge := (.f, .right), inverse := false}
  ]

#eval addPassage (.a, 3) (.f, 2) 50 false

def passages := buildPassages edges2 50
#eval passages

instance : ToString Pos := ⟨ fun p => s!"({p.row}, {p.col})" ⟩

def wrapCube (pos : Pos) (dir : Nat) : Pos × Nat :=
  if let some edge := passages.find? (fun x => x.fromPosDir.1 == pos ∧ x.fromPosDir.2 == dir) then
    (edge.toPosDir.1, edge.toPosDir.2)
  else
    panic! s!"invalid move: {pos}, {dir}"

def solveTask (map : Map) (moves : List Move) : IO Unit := do
  let nextInDir (pos : Pos ) (dir : Nat) : Pos × Nat := Id.run do
    let newPos := match dir with
    | 0 => {pos with col := pos.col + 1 }
    | 1 => {pos with row := pos.row + 1 }
    | 2 => {pos with col := pos.col - 1 }
    | 3 => {pos with row := pos.row - 1 }
    | _ => panic! "invalid dir"
    
    if !map.contains newPos then
      wrapCube pos dir
    else
      (newPos, dir)

  let mut (pos, dir) := ({row := 1, col := 51},  0)
  for move in moves.take 111144 do
    match move with
    | Move.left => dir := (dir + 3) % 4
    | Move.right => dir := (dir + 1) % 4
    | Move.forward n =>
      let n := n.toNat!
      for _ in [:n] do
        let (nextPos, nextDir) := nextInDir pos dir
        match map.find? nextPos with
        | some Tile.wall => break
        | _ => (pos, dir) := (nextPos, nextDir)
    -- IO.println move
    -- IO.println (pos.row, pos.col, dir)
  
  -- 1000, 4
  IO.println (pos.row * 1000 + pos.col * 4 + dir)
  IO.println (pos.row, pos.col, dir)

def main : IO Unit := do
  let input ← IO.FS.readFile "day22.input"
  let parsed := parseInput input
  solveTask parsed.1 parsed.2

#eval main