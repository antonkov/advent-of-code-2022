import Lean.Data.Parsec
import Mathlib.Init.Data.List.Basic

def input := "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"

inductive Signal where
| nat (n : Nat) 
| list (l : Array Signal)
deriving Inhabited

partial def stringFun : Signal → String
  | .nat n => toString n
  | .list l => l.map stringFun |>.toList.toString

instance : ToString Signal where
  toString := stringFun 

open Lean Parsec

def manySep (p : Parsec α) (sep : Parsec β) : Parsec (Array α) := do
  (do manyCore (sep *> p) #[← p]) <|> pure #[]

mutual
partial def parseNat := Signal.nat <$> String.toNat! <$> many1Chars digit
partial def parseList := (pchar '[') *> Signal.list <$> manySep parseSignal (pchar ',') <* pchar ']'
partial def parseSignal := parseNat <|> parseList
end

def parseTestCase (s : String) : Except String (Signal × Signal) := do
  if let [s1, s2] := s.splitOn "\n" then
    let r1 <- parseSignal.run s1
    let r2 <- parseSignal.run s2
    .ok ⟨ r1, r2 ⟩ 
  else
    .error "invalid input"


partial def compareSignal (s₁ s₂ : Signal) : Ordering :=
  match s₁, s₂ with
  | .nat n₁, .nat n₂ => compare n₁ n₂
  | .list l₁, .list l₂ => 
    let rec compareLists (l₁ l₂ : List Signal) : Ordering :=
      match l₁, l₂ with
      | [], [] => Ordering.eq
      | [], _ => Ordering.lt
      | _, [] => Ordering.gt
      | h₁::t₁, h₂::t₂ =>
        match compareSignal h₁ h₂ with
        | Ordering.eq => compareLists t₁ t₂
        | o => o

    compareLists l₁.toList l₂.toList
  | n@(.nat _), l@(.list _) => compareSignal (Signal.list #[n]) l
  | l@(.list _), n@(.nat _) => compareSignal l (Signal.list #[n])

def rightOrder (str₁ str₂ : String) : Except String Bool := do
  let s₁ ← parseSignal.run str₁
  let s₂ ← parseSignal.run str₂
  pure $ compareSignal s₁ s₂ == Ordering.lt

deriving instance Repr for Ordering


def main : IO Unit := do
  let input <- IO.FS.readFile "day13.input"
  let signals := input.splitOn "\n"
      |>.filter (· != "")
      |> fun l => "[[2]]"::"[[6]]"::l
      |>.map parseSignal.run
      |>.filterMap (·.toOption)

  let ordered := signals.toArray.qsort (fun s₁ s₂ => compareSignal s₁ s₂ == Ordering.lt)
  let findIdx (lStr : String) := do
    let l ← parseSignal.run lStr
    let res := ordered.findIdx? (fun s => compareSignal s l == Ordering.eq) |> (fun (x : Option Nat) => x.get! + 1)
    pure res
  let res₁ := findIdx "[[2]]"
  let res₂ := findIdx "[[6]]"
  IO.println (res₁, res₂)

#eval main
  