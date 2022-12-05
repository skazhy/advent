(ns advent.2022.test-day5
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2022.day5 :as d]))

(def ^:private example
  ["    [D]    "
   "[N] [C]    "
   "[Z] [M] [P]"
   " 1   2   3 "
   ""
   "move 1 from 2 to 1"
   "move 3 from 1 to 3"
   "move 2 from 2 to 1"
   "move 1 from 1 to 2"])

(deftest puzzle1
  (testing "Examples"
    (is (= "CMZ" (d/puzzle1 example))))

  (testing "Actual input"
    (is (= "MQTPGLLDN" (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= "MCD" (d/puzzle2 example))))

  (testing "Actual input"
    (is (= "LVZPSTTCZ" (d/puzzle2 d/puzzle-input)))))
