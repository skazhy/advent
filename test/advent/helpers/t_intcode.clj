(ns advent.helpers.t-intcode
  (:require [clojure.test :refer :all]
            [advent.helpers.intcode :refer [run-program]]))

(deftest comparison-tests
  (testing  "input equal to 8 with position mode"
    (is (= (:outputs (run-program [3 9 8 9 10 9 4 9 99 -1 8] 1)) [0]))
    (is (= (:outputs (run-program [3 9 8 9 10 9 4 9 99 -1 8] 8)) [1])))

  (testing "input less than 8 with position mode"
    (is (= (:outputs (run-program [3 9 7 9 10 9 4 9 99 -1 8] 1)) [1]))
    (is (= (:outputs (run-program [3 9 7 9 10 9 4 9 99 -1 8] 8)) [0])))

  (testing  "input equal to 8 with immediate mode"
    (is (= (:outputs (run-program [3 3 1108 -1 8 3 4 3 99] 1)) [0]))
    (is (= (:outputs (run-program [3 3 1108 -1 8 3 4 3 99] 8)) [1])))

  (testing "input less than 8 with immediate mode"
    (is (= (:outputs (run-program [3 3 1107 -1 8 3 4 3 99] 1)) [1]))
    (is (= (:outputs (run-program [3 3 1107 -1 8 3 4 3 99] 8)) [0]))))

(deftest jump-tests
  (testing "input equals to zero with position mode"
    (is (= (:outputs (run-program [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
                                  0))
           [0]))

    (is (= (:outputs (run-program [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9]
                                  4))
           [1])))

  (testing "input equals to zero with immediate mode"
    (is (= (:outputs (run-program [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] 0))
           [0]))

    (is (= (:outputs (run-program [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] 4))
           [1]))))

(deftest combined-example
  (is (= (:outputs (run-program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                                 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                                 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                                7))
         [999]))

  (is (= (:outputs (run-program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                                 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                                 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                                8))
         [1000]))
  (is (= (:outputs (run-program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                                 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                                 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]
                                80))
         [1001])))
