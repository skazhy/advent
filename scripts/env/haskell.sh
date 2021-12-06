# Haskell-specific methods for Advent of Code tooling

function setup {
  SRC_FILE="src/haskell/$YEAR/Day$DAY.hs"
  mkdir -p src/haskell/$YEAR/
}

function gen_src_file_content {
  cp src/haskell/2019/Day1.hs $SRC_FILE
  sed -i "" "s/Day1/Day$DAY/g; \
             3s/Day.*/$TITLE/; \
             5d; \
             6s:2019/day/1:$YEAR/day/$DAY:; \
             s/parsedInput (2019, 1) intLines/parsedInput ($YEAR, $DAY) lines/; \
             15,21d;  \
             24,25d;" $SRC_FILE
  echo "    print input" >> $SRC_FILE
}

function lint {
  hlint src/haskell/Advent.hs src/haskell/Data $SRC_FILE
}

function run_assert {
  GHC_FLAGS=""
  [[ "$LINT" ]] && GHC_FLAGS="-Werror -Wall -Wno-missing-signatures"
  ACTUAL=$(runghc $GHC_FLAGS -isrc/haskell $SRC_FILE)

  EXPECTED=$(cat "$SOLUTION_FILE")
  if [ "$ACTUAL" != "$EXPECTED" ]; then
    echo "Tests failed for $YEAR day $DAY:"
  else
    echo "Tests passed for $YEAR day $DAY:"
  fi

  act_lines=(`tr '\n' ' ' <<< "$ACTUAL"`)
  exp_lines=(`tr '\n' ' ' <<< "$EXPECTED"`)
  echo -n "Puzzle 1: "; echo_test_row "${exp_lines[0]}" "${act_lines[0]}"
  echo -n "Puzzle 2: "; echo_test_row "${exp_lines[1]}" "${act_lines[1]}"

  [[ "$ACTUAL" != "$EXPECTED" ]] && exit 1
}

function start_repl {
  ghci -isrc/haskell $SRC_FILE
}
