(ns advent.2017.test-day12
  (:require [clojure.test :refer :all]
            [advent.2017.day12 :as d]))

(def ^:private day12-example
  ["0 <-> 2"
   "1 <-> 1"
   "2 <-> 0, 3, 4"
   "3 <-> 2, 4"
   "4 <-> 2, 3, 6"
   "5 <-> 6"
   "6 <-> 4, 5"])

(deftest puzzle1
  (testing "Example"
    (is (= 6 (d/puzzle1 day12-example))))

  (testing "Actual input"
    (is (= 152 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Example"
    (is (= 2 (d/puzzle2 day12-example))))

  (testing "Actual input"
    (is (= 186 (d/puzzle2 d/puzzle-input)))))
