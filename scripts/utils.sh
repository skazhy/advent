function echo_test_row {
  if [ "$1" != "$2" ]; then
    echo -e "\033[0;32mExpected: $1\033[0m \033[0;31mGot: $2\033[0m"
  else
    echo -e "\033[0;32m$2\033[0m"
  fi
}

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

  exit 0
}

# evals a given template file and writes to the output path
# last line of template file is epxected to be a vim modeline
# and will be removed from the resulting output.
function eval_template {
  eval "echo \"$(sed '$d' <$1)\"" > $2
}
