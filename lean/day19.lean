import Lean.Data.Parsec
import Lean.Data.HashMap

open Lean Parsec
open Lean

def input := "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."

structure Blueprint where
  id : Nat
  oreRobotCost : Nat
  clayRobotCost : Nat
  obsidianRobotCost : Nat × Nat
  geodeRobotCost : Nat × Nat
deriving Repr, Inhabited

def parseBlueprint (s : String) : Option Blueprint :=
  let parseNat : Parsec Nat := do
    let n ← manyChars digit
    pure $ n.toNat!
  let parser : Parsec Blueprint := do
    skipString "Blueprint "
    let id ← parseNat
    skipString ": Each ore robot costs "
    let oreRobotCost ← parseNat
    skipString " ore. Each clay robot costs "
    let clayRobotCost ← parseNat
    skipString " ore. Each obsidian robot costs "
    let obsidianOre ← parseNat
    skipString " ore and "
    let obsidianClay ← parseNat
    skipString " clay. Each geode robot costs "
    let geodeOre ← parseNat
    skipString " ore and "
    let geodeObsidian ← parseNat
    skipString " obsidian."
    pure {
      id,
      oreRobotCost,
      clayRobotCost,
      obsidianRobotCost := (obsidianOre, obsidianClay),
      geodeRobotCost := (geodeOre, geodeObsidian)
    }
  parser.run s |>.toOption

structure State where
  ores : Nat
  clays : Nat
  obsidians : Nat
  oreRobots : Nat
  clayRobots : Nat
  obsidianRobots : Nat
  geodeRobots : Nat
deriving BEq, Hashable

instance : ToString State where
  toString s := s!"Ores: {s.ores}, Clays: {s.clays}, Obsidians: {s.obsidians}, Ore robots: {s.oreRobots}, Clay robots: {s.clayRobots}, Obsidian robots: {s.obsidianRobots}, Geode robots: {s.geodeRobots}"

def blueprints := input.splitOn "\n" |>.map parseBlueprint |>.filterMap id

def Lean.HashMap.updateWithMax (m : HashMap State Nat) (k : State) (newValue : Nat) : HashMap State Nat :=
  let value := if let some v := m.find? k then max v newValue else newValue
  m.insert k value

def solveTask (blueprint : Blueprint): IO Nat := do
  let numberMinutes := 24
  let mut dp : HashMap State Nat := HashMap.ofList [
    ({ores := 0, clays := 0, obsidians := 0, oreRobots := 1, clayRobots := 0, obsidianRobots := 0, geodeRobots := 0}, 0)
  ]
  for minute in [:numberMinutes] do
    IO.println s!"Minute {minute}"
    let mut nextDp : HashMap State Nat := HashMap.ofList []
    for (state, geodes) in dp.toList do
      let defaultState := {state with
        ores := state.ores + state.oreRobots,
        clays := state.clays + state.clayRobots,
        obsidians := state.obsidians + state.obsidianRobots
      }
      let newGeodes := geodes + state.geodeRobots
      -- Do nothing
      nextDp := nextDp.updateWithMax defaultState newGeodes
      if state.ores >= blueprint.oreRobotCost then
        let newState := {defaultState with
          ores := state.ores - blueprint.oreRobotCost + state.oreRobots,
          oreRobots := state.oreRobots + 1
        }
        nextDp := nextDp.updateWithMax newState newGeodes
      if state.ores >= blueprint.clayRobotCost then
        let newState := {defaultState with
          ores := state.ores - blueprint.clayRobotCost + state.oreRobots,
          clayRobots := state.clayRobots + 1
        }
        nextDp := nextDp.updateWithMax newState newGeodes
      if state.ores >= blueprint.obsidianRobotCost.1 && state.clays >= blueprint.obsidianRobotCost.2 then
        let newState := {defaultState with
          ores := state.ores - blueprint.obsidianRobotCost.1 + state.oreRobots,
          clays := state.clays - blueprint.obsidianRobotCost.2 + state.clayRobots,
          obsidianRobots := state.obsidianRobots + 1
        }
        nextDp := nextDp.updateWithMax newState newGeodes
      if state.ores >= blueprint.geodeRobotCost.1 && state.obsidians >= blueprint.geodeRobotCost.2 then
        let newState := {defaultState with
          ores := state.ores - blueprint.geodeRobotCost.1 + state.oreRobots,
          obsidians := state.obsidians - blueprint.geodeRobotCost.2 + state.obsidianRobots,
          geodeRobots := state.geodeRobots + 1
        }
        nextDp := nextDp.updateWithMax newState newGeodes
    dp := nextDp
  let max := dp.toList |>.map (fun x => x.2) |>.maximum?
  pure $ max.getD 0

def main := do
  let input ← IO.FS.readFile "day19.input"
  let blueprints := input.splitOn "\n" |>.map parseBlueprint |>.filterMap id
  let mut sum := 0
  for blueprint in blueprints do
    let result ← solveTask blueprint
    IO.println s!"Result for blueprint {blueprint.id}: {result}"
    sum := sum + result * blueprint.id
  IO.println s!"Sum: {sum}"
