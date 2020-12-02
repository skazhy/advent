(ns advent.2020.test-day2
  (:require [clojure.test :refer :all]
            [advent.2020.day2 :as d]))

(def ^:private example
  ["1-3 a: abcde"
   "1-3 b: cdefg"
   "2-9 c: ccccccccc"])

(deftest puzzle1
  (testing "Examples"
    (is (= 2 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 542 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 1 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 360 (d/puzzle2 d/puzzle-input)))))
