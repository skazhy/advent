#! /usr/bin/env bash

# One-stop shop for developing and testing AoC puzzles
# Usage: ./aoc.sh [hs|clj|rs] [[year] day] [lint] [test]

# When run with no options, will set up a puzzle in Clojure and open REPL

# Language-specific functions are defined in scripts/env/$language.sh:

# `setup`:  for defining TEST_FILE,SRC_FILE and setting up source directories
# `gen_src_file_content`: for setting up source and test file contents
# `lint`
# `start_repl`

set -e

YEAR=$(date "+%Y")
DAY=$(date "+%d" | sed -e 's/^0//g')
LANG="clj"

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
      LANG="hs"
      shift
      ;;
    rs)
      LANG="rs"
      shift
      ;;
    clj)
      LANG="clj"
      shift
      ;;
    doc)
      GEN_DOCS=1
      shift
      ;;
    src-path)
      ECHO_SRC_PATH=1
      SILENT=1
      shift
      ;;
    test-path)
      ECHO_TEST_PATH=1
      SILENT=1
      shift
      ;;
    input-path)
      ECHO_INPUT_PATH=1
      SILENT=1
      shift
      ;;
    solution-path)
      ECHO_SOLUTION_PATH=1
      SILENT=1
      shift
      ;;
  *)
    break
    ;;
  esac
done

# Handle numeric arguments to set year / day number.

if [[ "$NUMARG1" && "$NUMARG2" ]]; then
  YEAR=$NUMARG1
  DAY=$NUMARG2
fi

if [[ "$NUMARG1" && ! "$NUMARG2" ]]; then
  DAY=$NUMARG1
fi

# Environment setup

PUZZLE_URL="https://adventofcode.com/$YEAR/day/$DAY"
INPUT_FILE="resources/$YEAR/day$DAY.txt"
SOLUTION_FILE="resources/$YEAR/solutions/day$DAY.txt"
TITLE_CACHE=".titlecache"

if [[ "$LANG" = "clj" ]]; then
  source ./scripts/env/clojure.sh
fi

if [[ "$LANG" = "hs" ]]; then
  source ./scripts/env/haskell.sh
fi

if [[ "$LANG" = "rs" ]]; then
  source ./scripts/env/rust.sh
fi

mkdir -p resources/$YEAR/solutions
touch $TITLE_CACHE
setup

function gen_src_file {
  if [ ! -f "$SRC_FILE" ]; then
    if [ ! "$SILENT" ]; then
      echo "Creating new source files for $YEAR day $DAY..."
    fi

    # Look up puzzle title in title cache, fallback to fetching it from the website.
    TITLE=$(grep "$YEAR$DAY " $TITLE_CACHE | sed 's/^[0-9]* //g')
    if [ -z "$TITLE" ]; then
      TITLE=$(curl -s $PUZZLE_URL | grep -m1 h2 | sed 's/.*--- Day [0-9]*: \(.*\) ---.*/\1/')
      echo "$YEAR$DAY $TITLE" >> $TITLE_CACHE
    fi

    gen_src_file_content
    [ -f "$TEST_FILE" ] && git add --intent-to-add $TEST_FILE
    git add --intent-to-add $SRC_FILE

    NEW_FILE=1
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
    git add --intent-to-add $INPUT_FILE
  fi
}

function echo_test_row {
  if [ "$1" != "$2" ]; then
    echo -e "\033[0;32mExpected: $1\033[0m \033[0;31mGot: $2\033[0m"
  else
    echo -e "\033[0;32m$2\033[0m"
  fi
}

function gen_solution_file {
if [ ! -f "$SOLUTION_FILE" ]; then
  touch $SOLUTION_FILE
  git add --intent-to-add $SOLUTION_FILE
fi
}

[ "$LINT" ] && lint
[ "$ASSERT" ] && run_assert

if [[ "$GEN_DOCS" ]]; then
  # Regenerate completed puzzle doc
  ./scripts/completion.py
fi

if [[ "$LINT" || "$ASSERT" || "$GEN_DOCS" ]]; then
  exit 0
fi

if [ ! "$SILENT" ]; then
  echo "Puzzle details: $PUZZLE_URL"
fi

fetch_input_file
gen_src_file
gen_solution_file

if [ "$ECHO_SRC_PATH" ]; then
  echo "$SRC_FILE"
  exit 0
fi

if [ "$ECHO_TEST_PATH" ]; then
  echo "$TEST_FILE"
  exit 0
fi

if [ "$ECHO_INPUT_PATH" ]; then
  echo "$INPUT_FILE"
  exit 0
fi

if [ "$ECHO_SOLUTION_PATH" ]; then
  echo "$SOLUTION_FILE"
  exit 0
fi

start_repl
