(ns advent.2019.test-day7
  (:require [clojure.test :refer :all]
            [advent.2019.day7 :as d]))

(deftest ^:intcode puzzle1
  (testing "Examples"
    (is (= 43210 (d/puzzle1 [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])))

    (is (= 54321 (d/puzzle1 [3 23 3 24 1002 24 10 24 1002 23 -1 23
                             101 5 23 23 1 24 23 23 4 23 99 0 0])))

    (is (= 65210 (d/puzzle1 [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33
                             1002 33 7 33 1 33 31 31 1 32
                             31 31 4 31 99 0 0 0]))))

  (testing "Actual input"
    (is (= 34852 (d/puzzle1 d/puzzle-input)))))

#_ (deftest ^:intcode puzzle2
    (testing "Examples"
      (is (= 42 (d/puzzle2 "test"))))

    (testing "Actual input"
      (is (= 42 (d/puzzle2 d/puzzle-input)))))
