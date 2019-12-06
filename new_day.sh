#! /usr/bin/env bash

# ./new_day.sh [year day]

set -e

YEAR=$(date "+%Y")
DAY=$(date "+%d" | sed -e 's/0//g')

mkdir -p {src/advent,test/advent,resources}/$YEAR

# Gen source file

SRC_FILE=$(cat <<-eof
(ns advent.$YEAR.day$DAY
  "Advent of Code $YEAR, day $DAY: A Puzzle"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "$YEAR/day$DAY.txt" h/slurp-line))

(defn puzzle1 [input]

)

(defn puzzle2 [input]

)
eof
)

SRC_PATH="src/advent/$YEAR/day$DAY.clj"
[[ -f $SRC_PATH ]] || echo "$SRC_FILE" > $SRC_PATH
git add --intent-to-add $SRC_PATH

# Gen test file

TEST_FILE=$(cat <<-eof
(ns advent.$YEAR.test-day$DAY
  (:require [clojure.test :refer :all]
            [advent.$YEAR.day$DAY :as d]))

(deftest puzzle1
  (testing "Examples"
    (is (= 42 (d/puzzle1 "test"))))

  (testing "Actual input"
    (is (= 42 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 42 (d/puzzle2 "test"))))

  (testing "Actual input"
    (is (= 42 (d/puzzle2 d/puzzle-input)))))
eof
)

TEST_PATH=test/advent/$YEAR/test_day$DAY.clj
[[ -f $TEST_PATH ]] || echo "$TEST_FILE" > $TEST_PATH
git add --intent-to-add $TEST_PATH

# Resource

RESOURCE_PATH=resources/$YEAR/day$DAY.txt
touch $RESOURCE_PATH
git add --intent-to-add $RESOURCE_PATH
