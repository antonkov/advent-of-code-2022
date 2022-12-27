def input := "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122"

def Int.toSnafu (i : Int) : Char :=
  match i with
  | 0 => '0'
  | 1 => '1'
  | 2 => '2'
  | -1 => '-'
  | -2 => '='
  | _ => panic! "invalid snafu digit"

def Char.fromSnafu (c : Char) : Int :=
  match c with
  | '0' => 0
  | '1' => 1
  | '2' => 2
  | '-' => -1
  | '=' => -2
  | _ => panic! "invalid snafu digit"

def add (a b : List Char) : List Char :=
  let a' := a.reverse
  let b' := b.reverse
  let rec loop (a b : List Char) (carry : Int) : List Char :=
    match a, b with
    | [], [] => if carry != 0 then [carry.toSnafu] else []
    | [], b => if carry != 0 then loop [carry.toSnafu] b 0 else b
    | a, [] => if carry != 0 then loop a [carry.toSnafu] 0 else a
    | a::a', b::b' =>
      let sum := a.fromSnafu + b.fromSnafu + if carry != 0 then carry else 0
      let (value, carry) := if sum >= -2 && sum <= 2 then (sum.toSnafu, 0) else if sum < -2 then ((5 + sum).toSnafu, -1) else ((sum - 5).toSnafu, 1)
      value :: loop a' b' carry
  (loop a' b' 0).reverse
  decreasing_by sorry
  
def solveTask : IO Unit := do
  let lines ← IO.FS.lines "day25.input"
  let result := lines.map (fun s => s.toList) |>.foldl add ['0']
  IO.println (String.join $ result.map (·.toString))


-- #eval add ['1', '='] ['1', '2', '2']
#eval solveTask
  