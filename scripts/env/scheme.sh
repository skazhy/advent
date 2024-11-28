# Scheme-specific methods for Advent of Code tooling

source "./scripts/utils.sh"

function setup {
  SRC_FILE="src/scheme/$YEAR/day$DAY.scm"
  mkdir -p src/scheme/$YEAR
}

function gen_src_file_content {
  eval_template "scripts/templates/scheme.txt" $SRC_FILE
}

function run_puzzle {
  csi -ss $SRC_FILE $INPUT_FILE
}

function run_assert {
  assert $(csi -ss $SRC_FILE $INPUT_FILE)
}
