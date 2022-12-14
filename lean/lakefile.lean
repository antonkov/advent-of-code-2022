import Lake
open Lake DSL

package «advent» {
  -- add package configuration options here
}

@[default_target]
lean_exe «advent» {
  root := `day9
}

require std from git "https://github.com/leanprover/std4" @ "main"
require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "master"