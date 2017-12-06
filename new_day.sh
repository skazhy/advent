#! /usr/bin/env bash

# Boilerplate generation for Advent of Code.
# I am that lazy.

TODAY=`date "+%d" | sed -e 's/0//g'`
YEAR=`date "+%Y"`

echo "(ns advent.$YEAR.day$TODAY" > src/advent/$YEAR/day$TODAY.clj
echo "  \"Advent of Code $YEAR, day $TODAY\")" >> src/advent/$YEAR/day$TODAY.clj

touch resources/$YEAR/day$TODAY.txt

# Tests

TEST_FILE=test/advent/$YEAR/test_sample.clj
echo -e "\n" >> $TEST_FILE
echo "(deftest day-$TODAY-puzzles" >> $TEST_FILE
echo "  (testing \"Day $TODAY, first puzzle\")" >> $TEST_FILE
echo "  (testing \"Day $TODAY, second puzzle\"))" >> $TEST_FILE
