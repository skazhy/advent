# Clojure-specific methods for Advent of Code tooling

source "./scripts/utils.sh"

function setup {
  TEST_FILE="test/advent/$YEAR/test_day$DAY.clj"
  SRC_FILE="src/advent/$YEAR/day$DAY.clj"
  mkdir -p {src,test}/advent/$YEAR/
}

function gen_src_file_content {
  eval_template "scripts/templates/clojure_src.txt" $SRC_FILE

  SOLUTION1=42
  SOLUTION2=42

  # If solutions file is non-empty use actual solutions in the test file.
  if [[ -s "$SOLUTION_FILE" ]]; then
    sols=(`cat $SOLUTION_FILE | tr '\n' ' '`)

    SOLUTION1=${sols[0]}
    SOLUTION2=${sols[1]}
  fi

  eval_template "scripts/templates/clojure_test.txt" $TEST_FILE
}

function run_assert {
  clj -M:test -n advent.$YEAR.test-day$DAY
}

function lint {
  clj-kondo --lint $SRC_FILE $TEST_FILE
}

function start_repl {
  rlwrap clj -M:repl
}
