# Haskell-specific methods for Advent of Code tooling

source "./scripts/utils.sh"

function setup {
  SRC_FILE="src/haskell/$YEAR/Day$DAY.hs"
  mkdir -p src/haskell/$YEAR/
}

function gen_src_file_content {
  eval_template "scripts/templates/haskell.txt" $SRC_FILE
}

function lint {
  hlint src/haskell/Advent.hs src/haskell/Data $SRC_FILE
}

function run_assert {
  GHC_FLAGS=""
  [[ "$LINT" ]] && GHC_FLAGS="-Werror -Wall -Wno-missing-signatures"
  assert $(runghc $GHC_FLAGS -isrc/haskell $SRC_FILE)
}

function start_repl {
  ghci -isrc/haskell $SRC_FILE
}
