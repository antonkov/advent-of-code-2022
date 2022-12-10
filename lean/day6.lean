def tokenStart (s: String) :=
  let sortedList := s.toList.toArray.qsort (· < ·) |> Array.toList
  let neighbours := (sortedList.drop 1).zip sortedList.dropLast
  neighbours.all (fun (x: Char × Char) => x.1 ≠ x.2)

def size := 14

def String.groups (input: String) :=
  (λ i => (input.drop i).take size |> tokenStart) <$> (List.range (input.length - (size - 1)))

def main : IO Unit := do
  let input <- IO.FS.readFile "day6.input"
  IO.println $ input.groups.takeWhile not |> (List.length · + size)
