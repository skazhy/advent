(ns advent.2020.test-day8
  (:require [clojure.test :refer :all]
            [advent.2020.day8 :as d]))

(def ^:private example ["nop +0"
                        "acc +1"
                        "jmp +4"
                        "acc +3"
                        "jmp -3"
                        "acc -99"
                        "acc +1"
                        "jmp -4"
                        "acc +6"])

(deftest puzzle1
  (testing "Examples"
    (is (= 5 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 2003 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 8 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 1984 (d/puzzle2 d/puzzle-input)))))
