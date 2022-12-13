import Lake
open Lake DSL

package «advent» {
  -- add package configuration options here
}

@[default_target]
lean_exe «advent» {
  root := `day7
}

require std from git "https://github.com/leanprover/std4" @ "main"