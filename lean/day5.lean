def input := 
"
    [D]       
[N] [C]    
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"

structure Command where
  num : Nat
  fromSt : Nat
  toSt : Nat
deriving Repr, Inhabited

def List.transpose (l : List (List α)) : List (List α) := (l.foldr go #[]).toList where
  pop (old : List α) : StateM (List α) (List α)
    | [] => (old, [])
    | a :: l => (a :: old, l)

  go (l : List α) (acc : Array (List α)) : Array (List α) :=
    let (acc, l) := acc.mapM pop l
    l.foldl (init := acc) fun arr a => arr.push [a]

def takeFromStack (chars : List Char) (i : Nat) : Option Char :=
  match chars[4 * i + 1]? with
  | some ' ' => none
  | optC => optC

def parseLine (line : String) (numStacks : Nat) : List (Option Char) :=
  List.map (takeFromStack line.toList) (List.range numStacks)

def parseCommand (line : String) : Command := 
  let parts := line.splitOn " "
  let num := parts[1]!.toNat!
  let fromSt := parts[3]!.toNat! - 1
  let toSt := parts[5]!.toNat! - 1
  ⟨num, fromSt, toSt ⟩ 

def executeCommand (stacks : List (List Char)) (cmd : Command) : List (List Char) := 
  let fromSt := cmd.fromSt
  let toSt := cmd.toSt
  let num := cmd.num
  let fromStack := stacks[fromSt]!
  let toStack := stacks[toSt]!
  let fromStack' := List.drop num fromStack 
  let toStack' := (List.take num fromStack) ++ toStack

  let stacks' := stacks.set fromSt fromStack'
  stacks'.set toSt toStack'

def executeCommands (cmds : List Command) (stacks : List (List Char)) : List (List Char) := 
  cmds.foldl executeCommand stacks

def main : IO Unit := do
  let file <- IO.FS.readFile "day5.input"
  let splitInput := file.splitOn "\n\n"
  let stackLines := splitInput[0]!.splitOn "\n"
  let inputStacks := List.dropLast stackLines
  let numStacks := 9
  let stacks : List (List Char):= 
    let trStacks := List.map (parseLine · numStacks) inputStacks
    List.map (List.filterMap id) trStacks.transpose
  
  let commandLines := splitInput[1]!.splitOn "\n"
  let commands : List Command := List.map parseCommand commandLines

  let finalState := executeCommands commands stacks
  let answer := finalState.map (fun l => l[0]!.toString) |> String.join

  IO.println answer
