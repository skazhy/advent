# Clojure-specific methods for Advent of Code

function setup {
  TEST_FILE="test/advent/$YEAR/test_day$DAY.clj"
  SRC_FILE="src/advent/$YEAR/day$DAY.clj"
  mkdir -p {src,test}/advent/$YEAR/
}

function gen_src_file_content {
    src_content=$(cat <<-eof
(ns advent.$YEAR.day$DAY
  "Advent of Code $YEAR, day $DAY: $TITLE"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "$YEAR/day$DAY.txt" h/slurp-lines))

(defn puzzle1 [input]

)

(defn puzzle2 [input]

)
eof
)

  echo "$src_content" > $SRC_FILE

  test_content=$(cat <<-eof
(ns advent.$YEAR.test-day$DAY
  (:require [clojure.test :refer :all]
            [advent.$YEAR.day$DAY :as d]))

(def ^:private example "test")

(deftest puzzle1
  (testing "Examples"
    (is (= 42 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 42 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 42 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 42 (d/puzzle2 d/puzzle-input)))))
eof
)

  echo "$test_content" > $TEST_FILE
}

function lint {
  # TODO:
  return
}

function start_repl {
  lein repl
}
