(ns advent.2022.test-day1
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2022.day1 :as d]))

(def ^:private example
  ["1000" "2000" "3000" ""
   "4000" ""
   "5000" "6000" ""
   "7000" "8000" "9000" ""
   "10000" ""])

(deftest puzzle1
  (testing "Examples"
    (is (= 24000 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 74711 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 45000 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 209481 (d/puzzle2 d/puzzle-input)))))
