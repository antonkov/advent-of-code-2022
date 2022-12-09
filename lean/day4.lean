structure Range where
  min : Nat
  max : Nat
deriving Inhabited, Repr

def parseRange (s: String) : Range :=
  if let [a, b] := s.splitOn "-" then ⟨ a.toNat!, b.toNat! ⟩ 
  else panic! "invalid range"

def parseInput(s: String): List (Range × Range) := 
  let ranges := s.splitOn "\n"
  let parsePair := fun (s: String) =>
    if let [a, b] := s.splitOn ","
    then (parseRange a, parseRange b)
    else panic! "not pair"
  List.map parsePair ranges

def f (p: Range × Range): Bool := match p with
  | (⟨ a, b ⟩, ⟨ c, d ⟩) => a ≤ c ∧ d ≤ b ∨ c ≤ a ∧ b ≤ d

def g (p: Range × Range): Bool := match p with
  | (⟨ a, b ⟩, ⟨ c, d ⟩) => Nat.max b d - Nat.min a c + 1 < b - a + d - c + 2

#eval g ((⟨ 5, 7 ⟩, ⟨ 7, 9 ⟩))

def main : IO Unit := do
  let file ← IO.FS.readFile "day4.input"
  IO.println $ List.length $ List.filter g (parseInput file)