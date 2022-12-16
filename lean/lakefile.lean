import Lake
open Lake DSL

package «advent»

@[default_target]
lean_exe advent {
  root := `day16
}

require std from git "https://github.com/leanprover/std4" @ "main"
require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "master"