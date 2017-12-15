(ns advent.2017.test-day8
  (:require [clojure.test :refer :all]
            [clojure.java.io :refer [resource]]
            [advent.helpers :refer [slurp-word-lines split-lines]]
            [advent.2017.day8 :as d]))


(def ^:private day8-example
  (split-lines
    ["b inc 5 if a > 1"
     "a inc 1 if b < 5"
     "c dec -10 if a >= 1"
     "c inc -20 if c == 10"]))

(def ^:private day8-input (slurp-word-lines (resource "2017/day8.txt")))

(deftest puzzle1
  (testing "Example"
    (is (= 1 (d/puzzle1 day8-example))))

  (testing "Actual input"
    (is (= 3745 (d/puzzle1 day8-input)))))


(deftest puzzle2
  (testing "Example"
    (is (= 10 (d/puzzle2 day8-example))))

  (testing "Actual input"
    (is (= 4644 (d/puzzle2 day8-input)))))
