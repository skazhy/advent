#! /usr/bin/env bash

# Python-specific methods for Advent of Code tooling

source "./scripts/utils.sh"

function setup {
  mkdir -p src/python/$YEAR
  touch src/python/$YEAR/__init__.py
  git add -N src/python/$YEAR/__init__.py

  SRC_FILE="src/python/$YEAR/day$DAY.py"
}

function gen_src_file_content {
  eval_template "scripts/templates/python.txt" $SRC_FILE
}

function run_assert {
  assert $(PYTHONPATH=src/python python3 $SRC_FILE)
}

function lint {
  black $SRC_FILE src/python/helpers.py
}

function start_repl {
  PYTHONPATH=src/python python3 $SRC_FILE
}
