(ns advent.2020.test-day14
  (:require [clojure.test :refer :all]
            [advent.2020.day14 :as d]))

(def ^:private example ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                        "mem[8] = 11"
                        "mem[7] = 101"
                        "mem[8] = 0"])

(def ^:private example2 ["mask = 000000000000000000000000000000X1001X"
                         "mem[42] = 100"
                         "mask = 00000000000000000000000000000000X0XX"
                         "mem[26] = 1"])

(deftest puzzle1
  (testing "Examples"
    (is (= 165 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 14839536808842 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 208 (d/puzzle2 example2))))

  (testing "Actual input"
    (is (= 4215284199669 (d/puzzle2 d/puzzle-input)))))
