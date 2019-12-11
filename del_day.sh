#1 /usr/bin/env bash

# ./del_day year day

# Deletes all files associated with a day.

YEAR=$1
DAY=$2

rm -f \
  src/advent/$YEAR/day$DAY.clj \
  test/advent/$YEAR/test_day$DAY.clj \
  resources/$YEAR/day$DAY.txt
