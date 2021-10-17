# Haskell-specific methods for Advent of Code

function setup {
  SRC_FILE="src/haskell/$YEAR/Day$DAY.hs"
  mkdir -p src/haskell/$YEAR/
}

function gen_src_file_content {
  cp src/haskell/2019/Day1.hs $SRC_FILE
  # Remove / replace 2020.01 specific code.
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
  runghc $GHC_FLAGS -isrc/haskell $SRC_FILE
}

function start_repl {
  ghci -isrc/haskell $SRC_FILE
}
