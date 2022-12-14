import Std.Data.HashMap.Basic
import Mathlib.Data.Vector

def input := "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"

structure Pos where
  x : Int
  y : Int
deriving Repr, Inhabited, BEq, Hashable

instance : Repr Pos where
  reprPrec p _ := s!"({p.x}, {p.y})"

structure Command where
  dir : String
  len : Nat
deriving Repr, Inhabited

def dirToPos (dir : String) : Pos :=
  match dir with
  | "R" => { x := 1, y := 0 }
  | "L" => { x := -1, y := 0 }
  | "U" => { x := 0, y := 1 }
  | "D" => { x := 0, y := -1 }
  | _ => panic! "Invalid direction"

def dragTail (head tail : Pos) : Pos :=
  if (head.x - tail.x).natAbs <= 1 ∧ (head.y - tail.y).natAbs <= 1 then
    tail
  else if head.x = tail.x then
    { x := tail.x, y := tail.y + (if head.y > tail.y then 1 else -1) }
  else if head.y = tail.y then
    { x := tail.x + (if head.x > tail.x then 1 else -1), y := tail.y }
  else
    { x := tail.x + (if head.x > tail.x then 1 else -1), y := tail.y + (if head.y > tail.y then 1 else -1) }

abbrev Rope := List Pos
  
def solveTask (input: String) :=
  let commands := input.splitOn "\n" |>.map (λ line => if let [dir, len] := line.splitOn " " then Command.mk dir len.toNat! else panic! "Invalid input")
  let simpCommands := commands.toArray.concatMap (λ cmd => List.repeat (dirToPos cmd.dir) cmd.len |>.toArray)
  let ropePos := simpCommands.toList.scanl (λ (rope : Rope) cmd => 
    let newHead: Pos := { x := rope[0]!.x + cmd.x, y := rope[0]!.y + cmd.y }
    (rope.drop 1).foldl (fun front tail => front ++ [dragTail front.getLast! tail]) [newHead]
  ) (List.replicate 10 $ Pos.mk 0 0)
  let tailPos := ropePos.map (λ rope => rope.getLast! )
  tailPos.foldl (λ map pos => map.insert pos ()) Std.HashMap.empty |>.toArray |>.map (fun (k, _) => k) |>.size

#eval solveTask input

#eval dragTail (Pos.mk 4 2) (Pos.mk 3 0)
#eval dragTail (Pos.mk 0 0) (Pos.mk 2 2)
#eval dragTail (Pos.mk 0 0) (Pos.mk 2 0)
#eval dragTail (Pos.mk 0 0) (Pos.mk 0 2)

def main : IO Unit := do
  let input ← IO.FS.readFile "day9.input"
  IO.println $ solveTask input

#eval main