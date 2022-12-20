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
  clays : Nat
  obsidians : Nat
  geodes : Nat
  oreRobots : Nat
  clayRobots : Nat
  obsidianRobots : Nat
  geodeRobots : Nat
deriving BEq, Hashable

instance : ToString State where
  toString s := s!"Geodes: {s.geodes}, Clays: {s.clays}, Obsidians: {s.obsidians}, Ore robots: {s.oreRobots}, Clay robots: {s.clayRobots}, Obsidian robots: {s.obsidianRobots}, Geode robots: {s.geodeRobots}"

def blueprints := input.splitOn "\n" |>.map parseBlueprint |>.filterMap id

def Lean.HashMap.updateWithMax (m : HashMap State Nat) (k : State) (newValue : Nat) : HashMap State Nat :=
  let value := if let some v := m.find? k then max v newValue else newValue
  m.insert k value

def solveTask (blueprint : Blueprint): IO Nat := do
  let numberMinutes := 32
  let mut dp : HashMap State Nat := HashMap.ofList [
    ({geodes:= 0, clays := 0, obsidians := 0, oreRobots := 1, clayRobots := 0, obsidianRobots := 0, geodeRobots := 0}, 0)
  ]
  for minute in [:numberMinutes] do
    IO.println s!"Minute {minute}"
    let mut nextDp : HashMap State Nat := HashMap.ofList []
    for (state, ores) in dp.toList do
      let defaultState := {state with
        geodes := state.geodes + state.geodeRobots,
        clays := state.clays + state.clayRobots,
        obsidians := state.obsidians + state.obsidianRobots
      }
      -- Do nothing
      if ores >= blueprint.geodeRobotCost.1 && state.obsidians >= blueprint.geodeRobotCost.2 then
        let newState := {defaultState with
          obsidians := state.obsidians - blueprint.geodeRobotCost.2 + state.obsidianRobots,
          geodeRobots := state.geodeRobots + 1
        }
        let newOres := ores - blueprint.geodeRobotCost.1 + state.oreRobots
        nextDp := nextDp.updateWithMax newState newOres
      else
        nextDp := nextDp.updateWithMax defaultState (ores + state.oreRobots)

        if ores >= blueprint.oreRobotCost then
          let newState := {defaultState with
            oreRobots := state.oreRobots + 1
          }
          let newOres := ores - blueprint.oreRobotCost + state.oreRobots
          nextDp := nextDp.updateWithMax newState newOres
        if ores >= blueprint.clayRobotCost then
          let newState := {defaultState with
            clayRobots := state.clayRobots + 1
          }
          let newOres := ores - blueprint.clayRobotCost + state.oreRobots
          nextDp := nextDp.updateWithMax newState newOres
        if ores >= blueprint.obsidianRobotCost.1 && state.clays >= blueprint.obsidianRobotCost.2 then
          let newState := {defaultState with
            clays := state.clays - blueprint.obsidianRobotCost.2 + state.clayRobots,
            obsidianRobots := state.obsidianRobots + 1
          }
          let newOres := ores - blueprint.obsidianRobotCost.1 + state.oreRobots
          nextDp := nextDp.updateWithMax newState newOres
    dp := nextDp
  let max := dp.toList |>.map (fun x => x.1.geodes) |>.maximum?
  pure $ max.getD 0

def main := do
  let input ← IO.FS.readFile "day19.input"
  let blueprints := input.splitOn "\n" |>.map parseBlueprint |>.filterMap id
  let mut answer := 1
  for blueprint in blueprints.take 3 do
    let result ← solveTask blueprint
    IO.println s!"Result for blueprint {blueprint.id}: {result}"
    answer := answer * result
  IO.println s!"Answer: {answer}"