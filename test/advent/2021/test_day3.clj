(ns advent.2021.test-day3
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2021.day3 :as d]))

(def ^:private example
  ["00100" "11110" "10110" "10111" "10101" "01111"
   "00111" "11100" "10000" "11001" "00010" "01010"])

(deftest puzzle1
  (testing "Examples"
    (is (= 198 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 3958484 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 230 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 1613181 (d/puzzle2 d/puzzle-input)))))
