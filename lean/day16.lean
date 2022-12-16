import Lean.Data.Parsec
import Lean.Data.HashMap
import Lean.Data.HashSet
import Std.Data.Array.Init.Basic
import Std.Data.List.Basic

open Lean Parsec
open Lean

def input := "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"

structure Valve where
  flowRate : Nat
  tunnels : List String
  profile : UInt64
deriving Repr, Inhabited

def manySep (p : Parsec α) (sep : Parsec β) : Parsec (Array α) := do manyCore (sep *> p) #[← p]

def parseValve (idx : Nat) (s : String) : Option (String × Valve) :=
  let parseName : Parsec String := do pure s!"{← anyChar}{← anyChar}"
  let parseNat: Parsec Nat := do
    let digits ← many1Chars digit
    pure digits.toNat!

  let parser : Parsec (String × Valve) := do
    skipString "Valve "
    let name ← parseName
    skipString " has flow rate="
    let flowRate ← parseNat
    skipString "; tunnels lead to " <|> skipString "; tunnel leads to "
    skipString "valves " <|> skipString "valve "
    let tunnels ← manySep parseName (pstring ", ")
    pure (name, { flowRate, tunnels := tunnels.toList, profile := (1 : UInt64).shiftLeft idx.toUInt64 })
  parser.run s |>.toOption

structure State where
  pos: List String
  openValves : UInt64

instance : BEq State where
  beq {s1 s2} := s1.pos == s2.pos && s1.openValves == s2.openValves

instance : Hashable State where
  hash {s} := (s.pos.map hash) ++ [hash s.openValves] |>.foldl mixHash 0

def List.sort (l : List String) : List String := (l.toArray |>.qsort (· < ·)).toList

def solveTask (valvesMap : HashMap String Valve) := do
  let n := 26
  let flows := valvesMap.toList.map (fun (x : String × Valve) => x.snd.flowRate)

  let maxCanGet (day: Nat) : Nat := Id.run do
    (n - day - 1) * flows.foldl (· + ·) 0

  IO.println s!"Max can get: {maxCanGet 0}"

  let mut dp : Array (HashMap State Nat) := Array.ofFn (fun (_ : Fin (n + 1)) => .empty)
  dp := dp.modify 0 (λ h => h.insert ⟨["AA", "AA"] , 0⟩ 0)
  for day in [0:n] do
    let currentMax := dp[day]!.toList |>.map Prod.snd |>.maximum?.getD 0
    -- IO.println s!"Current max: {currentMax}"
    for (state, value) in dp[day]!.toList do
      if value + maxCanGet day < currentMax then
        continue
      -- Generate state where we can get to from the current state.
      let genNewStates (state : State) (pressure : Nat) (me : Nat) : List (State × Nat) :=
        let valve := valvesMap.find! state.pos[me]!
        let tunnels : List (State × Nat) := valve.tunnels.map ({ pos := state.pos.set me ·, openValves := state.openValves }, pressure)
        if valve.flowRate > 0 && state.openValves &&& valve.profile == 0 then
          let newState : State := { pos := state.pos, openValves := state.openValves ||| valve.profile }
          (newState, pressure + valve.flowRate * (n - day - 1))::tunnels
        else tunnels

      for (state₁, value₁) in genNewStates state value 0 do
        for (newState, newValue) in genNewStates state₁ value₁ 1 do
          if dp[day + 1]!.findD newState 0 ≤ newValue then
            dp := dp.modify (day + 1) (λ h => h.insert newState newValue)
          else pure ()
    -- IO.println s!"Day {day}: {dp[day]!.size}"
  
  IO.println s!"{dp[n]!.toList |>.map Prod.snd |>.maximum?}"

def valves := input.splitOn "\n" |>.mapIdx parseValve |>.filterMap id
def valvesMap : HashMap String Valve := HashMap.ofList valves
#eval solveTask valvesMap

def main : IO Unit := do
  let input ← IO.FS.readFile "day16.input"
  let valves := input.splitOn "\n" |>.mapIdx parseValve |>.filterMap id
  -- IO.println $ valves.map (fun s => s.snd.flowRate) |>.filter (· != 0) |>.toArray.qsort (· > ·) |>.toList
  let valvesMap : HashMap String Valve := HashMap.ofList valves
  solveTask valvesMap

-- #eval main

