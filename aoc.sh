#! /usr/bin/env bash

# One-stop shop for developing and testing AoC puzzles
# Usage: ./aoc.sh [[language] [[year] day] [lint] [test]|doc]

# See doc/DEV.md for more information about extending this script.

set -e

YEAR=$(date "+%Y")
DAY=$(date "+%d" | sed -e 's/^0//g')
MONTH=$(date "+%m")
TITLE_CACHE=".aoccache/titles"

while :
do
  case "${1-}" in
    [0-9]*)
      [ "$NUMARG1" ] && NUMARG2=$1 || NUMARG1=$1
      shift
      ;;
    lint)
      LINT=1
      shift
      ;;
    test)
      ASSERT=1
      shift
      ;;
    hs)
      source ./scripts/env/haskell.sh
      shift
      ;;
    py)
      source ./scripts/env/python.sh
      shift
      ;;
    rs)
      source ./scripts/env/rust.sh
      shift
      ;;
    clj)
      source ./scripts/env/clojure.sh
      shift
      ;;
    idr)
      source ./scripts/env/idris.sh
      shift
      ;;
    doc)
      GEN_DOCS=1
      shift
      ;;
    srcpath)
      SRC_PATH=1
      shift
      ;;
    setup)
      SETUP=1
      shift
      ;;
    run)
      RUN=1
      shift
      ;;
    vim)
      VIM=1
      shift
      ;;
  *)
    break
    ;;
  esac
done

mkdir -p .aoccache

if [[ "$GEN_DOCS" ]]; then
  # Regenerate completed puzzle doc
  PYTHONPATH=scripts ./scripts/gen_docs.py "$@"
  mdformat README.md doc
  exit 0
fi

# Handle numeric arguments to set year / day number.

if [[ ! "$NUMARG2" && "$MONTH" != "12" ]]; then
  echo "Please provide year & month for the puzle!"
  exit 1
fi

if [[ "$NUMARG1" && "$NUMARG2" ]]; then
  YEAR=$NUMARG1
  DAY=$NUMARG2
fi

if [ "${#YEAR}" -eq 2 ]; then
  YEAR="20$YEAR"
fi

if [[ "$NUMARG1" && ! "$NUMARG2" ]]; then
  DAY=$NUMARG1
fi

mkdir -p resources/$YEAR/solutions

PUZZLE_URL="https://adventofcode.com/$YEAR/day/$DAY"
INPUT_FILE="resources/$YEAR/day$DAY.txt"
SOLUTION_FILE="resources/$YEAR/solutions/day$DAY.txt"

setup
if [ ! "$SRC_FILE" ]; then
  echo "Please select a languate!"
  exit 1
fi

function gen_src_file {
  touch $TITLE_CACHE

  if [ ! -f "$SRC_FILE" ]; then
    echo "Creating new source files for $YEAR day $DAY..."
    TITLE=$(bb ./scripts/utils.bb title $YEAR $DAY)

    gen_src_file_content
    [ -f "$TEST_FILE" ] && git add --intent-to-add $TEST_FILE
    git add --intent-to-add $SRC_FILE
  fi
}

function fetch_input_file {
  set -e
  if [ ! -f "$INPUT_FILE" ]; then
    if [ ! -f ".cookie" ]; then
      echo "Please save the cookie header value in .cookie!"
      exit 1
    fi
    curl --fail -s "$PUZZLE_URL/input" -H "Cookie: $(cat .cookie)" > "$INPUT_FILE" || \
      (echo "Error: please refresh .cookie!" && rm -f "$INPUT_FILE" && exit 1)
  fi
}

function gen_solution_file {
  if [ ! -f "$SOLUTION_FILE" ]; then
    touch $SOLUTION_FILE
  fi
}

[ "$LINT" ] && lint
[ "$ASSERT" ] && run_assert
[ "$SRC_PATH" ] && echo $SRC_FILE

if [[ "$LINT" || "$ASSERT" || "$SRC_PATH" ]]; then
  exit 0
fi

fetch_input_file
gen_src_file
gen_solution_file

if [[ "$SETUP" ]]; then
  exit 0
fi

if [[ "$VIM" ]]; then
  vim $SRC_FILE
 exit 0
fi

[[ $(type -t run_puzzle) == function && "$RUN" ]] && run_puzzle && exit 0

echo "Puzzle details: $PUZZLE_URL"
[[ $(type -t start_repl) == function ]] && start_repl
