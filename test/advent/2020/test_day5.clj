(ns advent.2020.test-day5
  (:require [clojure.test :refer :all]
            [advent.2020.day5 :as d]))

(deftest puzzle1
  (testing "Examples"
    (is (= 357 (d/decode-boarding-pass "FBFBBFFRLR")))
    (is (= 567 (d/decode-boarding-pass "BFFFBBFRRR")))
    (is (= 119 (d/decode-boarding-pass "FFFBBBFRRR")))
    (is (= 820 (d/decode-boarding-pass "BBFFBBFRLL"))))

  (testing "Actual input"
    (is (= 806 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Actual input"
    (is (= 562 (d/puzzle2 d/puzzle-input)))))
