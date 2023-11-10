# Idris-specific methods for Advent of Code tooling

source "./scripts/utils.sh"

function setup {
  SRC_FILE="src/idris/$YEAR/Day$DAY.idr"
  mkdir -p src/idris/$YEAR/
}

function gen_src_file_content {
  eval_template "scripts/templates/idris.txt" $SRC_FILE
}

function start_repl {
    idris2 $SRC_FILE -o Day$DAY
    ./build/exec/Day$DAY
}

function run_assert {
  assert $(start_repl)
}
