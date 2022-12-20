def input := "1
2
-3
3
-2
0
4"

def decKey : Int := 811589153

def solveTask (input : String) : IO Unit := do
  let xs := input.splitOn "\n" |>.map (fun x => x.toInt!)
  let n := xs.length
  let mut ys := (xs.map (· * decKey)).zip (List.range xs.length) |>.toArray
  for _ in [:10] do
    for i in [:n] do
      if let some idx := ys.findIdx? (fun x => x.2 == i) then
        let x := ys[idx]!.1
        let val := ((x % (n - 1) + n - 1) % (n - 1)).toNat
        let new : Array (Int × Nat) := ys[idx + 1:] ++ ys[:idx]
        ys := new[:val] ++ #[ys[idx]!] ++ new[val:]
  let zIdx := ys.findIdx? (fun x => x.1 == 0) |>.get!
  let idxs := [1000, 2000, 3000]
  IO.println $ idxs.map (fun i => ys[(zIdx + i) % n]!.1) |>.foldl Int.add 0

def main : IO Unit := do
  let input ← IO.FS.readFile "day20.input"
  solveTask input