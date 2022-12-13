import Init.Data.List
import Std.Data.List.Basic

def visible (l : List Int) : List Bool :=
  l.mapWithPrefixSuffix (fun pref x suf => x > pref.maximum?.getD (-1) ∨ x > suf.maximum?.getD (-1))

def score (l : List Int) : List Nat :=
  let canSee (l : List Int) (x : Int) : Nat :=
    let len :=  l.takeWhile (· < x) |>.length
    if len == l.length then len else len + 1
  l.mapWithPrefixSuffix (fun pref x suf => (canSee suf x) * (canSee pref.reverse x))

def parseInput (inputString : String): List (List Int) :=
  inputString.splitOn "\n" |>.map (fun s => s.toList.map (fun c => c.toNat - '0'.toNat))

def maxScore (inputString: String) :=
  let input := parseInput inputString
  let rowScore := input.map score
  let colScore := input.transpose.map score |>.transpose
  let scores : (List (List Nat)) := rowScore.zipWith (·.zipWith (· * ·) ·) colScore
  scores.map (fun x => x.maximum?.getD 0) |>.maximum?.getD 0

def inputString := "30373
25512
65332
33549
35390"

def numberVisible (inputString: String) : Nat :=
  let input := parseInput inputString
  let rowVis := input.map visible
  let colVis := input.transpose.map visible |>.transpose
  let vis : (List (List Bool)) := rowVis.zipWith (·.zipWith (· || ·) ·) colVis
  vis.foldl (fun acc x => acc + (x.filter id).length) 0

def main : IO Unit := do
  let input ← IO.FS.readFile "day8.input"
  IO.println (maxScore input)