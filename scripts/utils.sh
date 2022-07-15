# Compares contents of $SOLUTION_FILE to first 2 arguments
# passed to the function.
function assert {
  EXPECTED=$(cat "$SOLUTION_FILE")
  ACTUAL="$1
$2"
  if [ "$EXPECTED" != "$ACTUAL" ]; then
    echo "Tests failed for $YEAR day $DAY:"
  else
    echo "Tests passed for $YEAR day $DAY:"
  fi

  exp_lines=(`tr '\n' ' ' <<< "$EXPECTED"`)
  echo -n "Puzzle 1: "; echo_test_row "${exp_lines[0]}" "$1"
  echo -n "Puzzle 2: "; echo_test_row "${exp_lines[1]}" "$2"

  [[ "$ACTUAL" != "$EXPECTED" ]] && exit 1
}

# evals a given template file and writes to the output path
# last line of template file is epxected to be a vim modeline
# and will be removed from the resulting output.
function eval_template {
  eval "echo \"$(sed '$d' <$1)\"" > $2
}
