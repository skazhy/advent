(ns advent.2018.test-day1
  (:require [clojure.test :refer :all]
            [advent.2018.day1 :as d]))

(deftest puzzle1
  (testing "Examples"
    (is (= 3 (d/puzzle1 [1 -2 3 1])))
    (is (= 3 (d/puzzle1 [1 1 1])))
    (is (= -6 (d/puzzle1 [-1 -2 -3]))))

  (testing "Actual input"
    (is (= 576 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 2 (d/puzzle2 [1 -2 3 1])))
    (is (zero? (d/puzzle2 [1 -1])))
    (is (= 10 (d/puzzle2 [3 3 4 -2 -4])))
    (is (= 5 (d/puzzle2 [-6 3 8 5 -6])))
    (is (= 14 (d/puzzle2 [7 7 -2 -7 -4]))))

  (testing "Actual input"
    (is (= 77674 (d/puzzle2 d/puzzle-input)))))
