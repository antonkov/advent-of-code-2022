import Lean.Data.Parsec

def input := "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"

inductive Token where
  | cd : String → Token
  | ls
  | dir : String → Token
  | file : Nat → String → Token
deriving Inhabited, Repr

open Lean.Parsec

def anyString := manyChars anyChar

def parseCd := do
  _ ← pstring "$ cd "
  pure $ Token.cd (← anyString)

def parseName := do
  let ds ← many1Chars digit
  skipChar ' '
  pure $ Token.file ds.toNat! (← anyString) 

def parseDir := do
  _ ← pstring "dir "
  pure $ Token.dir (← anyString)

def parseLs := do
  _ ← pstring "$ ls"
  pure Token.ls

def parseToken := parseCd <|> parseLs <|> parseDir <|> parseName

def tokens (input: String) := parseToken.run <$> input.splitOn "\n" |> List.filterMap (·.toOption)

#eval (tokens input).take 5

def sum (l : List Nat) := l.foldl (· + ·) 0

structure State where
  sum : Nat
  sizes : List Nat
  rest : List Token
deriving Repr, Inhabited

mutual
partial def goSubDirs (ts : List Token) : State :=
  match ts with
  | [] => ⟨ 0, [], [] ⟩
  | (Token.cd "..")::xs => ⟨ 0, [], xs ⟩
  | _::xs => let dir := goDir xs
             let subdirs := goSubDirs dir.rest
             ⟨ dir.sum + subdirs.sum, subdirs.sizes ++ dir.sizes, subdirs.rest ⟩ 

partial def goDir (ts : List Token) : State :=
  let (curDir, rest) := List.span (fun | Token.cd _ => false | _ => true) ts 
  let files := curDir.filterMap (fun | Token.file size _ => some size | _ => none) 
  let ⟨ sum1, sizes, next ⟩ := goSubDirs rest
  let size := sum files
  ⟨ sum1 + size, (sum1 + size)::sizes, next ⟩ 
end

#eval goDir ((tokens input).drop 1)

def sums (input : String) := goDir ((tokens input).drop 1)

def toDelete (input : String) := 30000000 - (70000000 - (sums input).sum)

#eval (sums input).sizes.filter (· >= (toDelete input)) |> List.minimum?

#eval sums input

def main : IO Unit := do
  let input ← IO.FS.readFile "day7.input"
  let needToFree := toDelete input
  IO.println needToFree
  let sizes := (sums input).sizes.filter (· >= needToFree)
  IO.println sizes.minimum?