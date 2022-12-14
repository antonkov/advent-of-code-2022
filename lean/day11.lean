import Std.Data.List.Basic
import Mathlib.Init.Data.List.Basic

def input := "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"

inductive Operand where
  | num (n : Nat)
  | old
deriving Repr

inductive Operator where
  | mul
  | add
deriving Repr

structure Operation where
  operator : Operator
  operand : Operand
deriving Repr

def executeOperation (oper : Operation) (old : Nat) : Nat :=
  match oper with
  | ⟨Operator.mul, Operand.num n⟩ => old * n
  | ⟨Operator.mul, Operand.old⟩ => old * old
  | ⟨Operator.add, Operand.num n⟩ => old + n
  | ⟨Operator.add, Operand.old⟩ => old + old


def parseOperation (oper : String) : Option Operation := do
  let opPrts := oper.splitOn " "
  let operator <- match opPrts[3]! with
    | "*" => Operator.mul
    | "+" => Operator.add
    | _ => none
  let operand <-  match opPrts[4]! with
    | "old" => Operand.old
    | n => if let some n := n.toNat? 
            then Operand.num n 
            else none
  pure ⟨operator, operand⟩

structure Monkey where
  idx : Nat
  operation : Operation 
  test : Nat
  ifTrue : Nat 
  ifFalse : Nat 
deriving Repr

def parseMonkey (s : String): Option (Monkey × List Nat) := do
  let extractAfter (s mid : String) := s.splitOn mid |> (fun s => s[1]!)

  if let [header, startingItems, operation, test, ifTrue, ifFalse] := s.splitOn "\n" then
    (
      Monkey.mk
        ((extractAfter header "Monkey ").dropRight 1).toNat!
        (← parseOperation $ extractAfter operation ": ")
        (extractAfter test "divisible by ").toNat!
        (extractAfter ifTrue "monkey ").toNat!
        (extractAfter ifFalse "monkey ").toNat!,
      (
        startingItems.splitOn ":" 
          |> (fun s => (s[1]! : String).splitOn ",") 
          |>.map (fun (s: String) => s.trim.toNat!)
      )
    )
  else
    none

structure Transition where
  to : Nat
  worry : Nat
deriving Repr

def main : IO Unit := do
  let input <- IO.FS.readFile "day11.input"
  let monkeysStr := input.splitOn "\n\n"
  let parseRes := monkeysStr.map parseMonkey
    |> List.filterMap id
  let monkeys := parseRes.map (·.1)
  let modulo := monkeys.map (·.test) |>.foldl (· * ·) 1
  let executeMonkey (m : Monkey) (worries: List Nat) : List Transition :=
    let newWorries := worries.map (fun t => executeOperation m.operation t |> (· % modulo))
    let (toTrue, toFalse) := newWorries.partition (fun w => w % m.test == 0)
    toTrue.map (⟨m.ifTrue, ·⟩) ++ toFalse.map (⟨m.ifFalse, ·⟩)
  let transitions : List Transition := parseRes.map (fun (m, tr) => tr.map (⟨m.idx, ·⟩)) |>.join
  let doRound (input: List Transition × Array Nat) : List Transition × Array Nat :=
    monkeys.foldl
    (fun (tr, cnts) m => 
      let ⟨ myTrans, otherTrans ⟩ := tr.partition (fun t => t.to == m.idx) 
      (otherTrans ++ executeMonkey m (myTrans.map (·.worry)), cnts.modify m.idx (. + myTrans.length))
    ) input
  let allTr := (List.range 10000).foldl (fun tr _ => doRound tr) (transitions , List.repeat 0 monkeys.length |>.toArray)
  let result := allTr.2.qsort (· < ·) |>.reverse.toList.take 2 |>.foldl (· * ·) 1
  IO.println result

#eval main