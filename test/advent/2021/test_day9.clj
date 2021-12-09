(ns advent.2021.test-day9
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2021.day9 :as d]))

(def ^:private example
  ["2199943210" "3987894921" "9856789892" "8767896789" "9899965678"])

(deftest puzzle1
  (testing "Examples"
    (is (= 15 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 577 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 1134 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 1069200 (d/puzzle2 d/puzzle-input)))))
