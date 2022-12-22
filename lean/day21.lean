import Lean.Data.HashMap

open Lean

def input := "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"

inductive Monkey where
  | number (n : Int)
  | operation (l r : String) (op : Int → Int → Int)

def Monkey.toString : Monkey → String
  | Monkey.number n => s!"{n}"
  | Monkey.operation l r .. => s!"({l} {r})"

def parseInput (input : String) : HashMap String Monkey := Id.run do
  let parseLine (s : String) : Option (String × Monkey) :=
    if let [name, expr] := s.splitOn ": " then
      if let [l, op, r] := expr.splitOn " " then
        let op := match op with
          | "+" => Int.add
          | "-" => Int.sub
          | "*" => Int.mul
          | "/" => Int.div
          | _ => panic! "unknown op"
        some $ (name, Monkey.operation l r op)
      else
        some $ (name, Monkey.number expr.toNat!)
    else
      none

  let monkeys := input.splitOn "\n" |>.map parseLine |>.filterMap id
  HashMap.ofList monkeys

partial def eval (monkeys : HashMap String Monkey) (name : String) :=
  match monkeys.find? name with
  | none => panic! "unknown monkey"
  | some (Monkey.number n) => n
  | some (Monkey.operation l r op) =>
      let l := eval monkeys l
      let r := eval monkeys r
      op l r

def solveTask (monkeys : HashMap String Monkey) : List (Int × Int) :=
  match monkeys.find? "root"  with
  | none => panic! "no root"
  | some (Monkey.number ..) => panic! "root is a number"
  | some (Monkey.operation lmonkey rmonkey ..) =>
    -- binsearch when l == r on a from 0 to 3497999290409
    Id.run do
      let mut l := 0
      let mut r := 3497999290409
      while r - l > 1 do
        let a := (l + r) / 2
        let monkeys := monkeys.insert "humn" (Monkey.number a)
        let lres := eval monkeys lmonkey
        let rres := eval monkeys rmonkey
        if lres < rres then
          r := a
        else
          l := a

      let monkeys := monkeys.insert "humn" (Monkey.number l)
      [(l, r), (eval monkeys lmonkey, eval monkeys rmonkey)]

def main : IO Unit := do
  let input ← IO.FS.readFile "day21.input"
  let result := solveTask $ parseInput input
  IO.println result

#eval (58602687946509 - 23622695042414) / 10 
#eval 58602687946509 - 58602687945894

#eval main
def monkeys := parseInput input
#eval solveTask monkeys