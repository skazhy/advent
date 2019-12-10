(ns advent.2019.test-day9
  (:require [clojure.test :refer :all]
            [advent.2019.day9 :as d]))

(deftest ^:intcode puzzle1
  (testing "Examples"
    (is (= 1219070632396864 (d/puzzle1 [1102 34915192 34915192 7 4 7 99 0])))

    (is (= 1125899906842624 (d/puzzle1 [104 1125899906842624 99]))))

  (testing "Actual input"
    (is (= 2316632620 (d/puzzle1 d/puzzle-input)))))

(deftest ^{:slow true :intcode true} puzzle2
  (is (= 78869 (d/puzzle2 d/puzzle-input))))
