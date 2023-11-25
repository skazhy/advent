#! /usr/bin/env bash

# Rust-specific methods for Advent of Code tooling

source "./scripts/utils.sh"

function setup {
  mkdir -p src/rust/year$YEAR

  SRC_FILE="src/rust/year$YEAR/day$DAY.rs"
  MAIN_FILE="src/rust/main.rs"
  if [[ "$ASSERT" ]]; then
    cargo build -q
  fi
}

function gen_src_file_content {
  eval_template "scripts/templates/rust_src.txt" $SRC_FILE

  # Update mod.rs file for the given year's module.
  MOD_FILE="src/rust/year$YEAR/mod.rs"
  touch $MOD_FILE
  git add -N $MOD_FILE
  MOD="pub mod day$DAY;"
  if [[ ! $(grep "$MOD" "$MOD_FILE") ]]; then
    echo "$MOD" >> $MOD_FILE
    sort -o $MOD_FILE $MOD_FILE
  fi

  # Update main.rs
  MODS=$(cd src/rust; ls -1d year*/ | sed 's/^/mod /g; s/\/$/;/g')
  PUZZLES=$(cd src/rust; ls -1 */day*.rs | sed -E 's/year([0-9]+)\/day([0-9]+).rs/("\1", "\2") => year\1::day\2::run,/g')
  eval_template "scripts/templates/rust_main.txt" $MAIN_FILE
  rustfmt $MAIN_FILE
}

function run_assert {
  grep "cfg(test)" $SRC_FILE && cargo test year$YEAR::day$DAY
  if [[ "$YEAR" == "2019" ]]; then
    cargo test intcode
  fi

  assert $(./target/debug/advent $YEAR $DAY)
}

function run_puzzle {
  cargo run $YEAR $DAY
}

function lint {
  rustfmt $MAIN_FILE
  rustfmt $SRC_FILE
}
