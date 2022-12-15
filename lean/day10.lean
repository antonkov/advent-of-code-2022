import Std.Data.List.Basic 

def input := "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"

inductive Command where
| noop : Command
| addx : Int → Command
deriving Inhabited, Repr

def parseCommand (s : String) : Command :=
  match s.splitOn " " with
  | ["noop"] => Command.noop
  | ["addx", x] => Command.addx x.toInt!
  | _ => panic! "invalid command"


def solveTask (input: String) :=
  -- let cycles : List Nat := [20, 60, 100, 140, 180, 220]
  let commands := input.splitOn "\n" |>.map parseCommand
  let uniformCommands := commands.map (fun
    | Command.addx x => [Command.noop, Command.addx x]
    | c => [c]) |>.join

  let values := uniformCommands.scanl (fun acc c => match c with
    | Command.noop => acc
    | Command.addx x => acc + x) (1: Int)

  values
  -- cycles |>.map (fun v  => values[v - 1]! * v) |>.foldl (· + ·) 0

def crt (input : String) := solveTask input |>.mapIdx (fun idx (value : Int) => if (value - (idx % 40)).natAbs <= 1 then "#" else ".")
def printRow (crt : List String) (row : Nat) :=
  crt.drop (row * 40) |>.take (40)

def main : IO Unit := do
  let input <- IO.FS.readFile "day10.input"
  let s := crt input
  (List.range 6).forA (fun i => IO.println $ (printRow s i |> String.join))
  
#eval main

-- Documentation https://leanprover-community.github.io/mathlib4_docs/
-- https://adventofcode.com/2022/day/10