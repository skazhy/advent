(ns advent.2020.test-day18
  (:require [clojure.test :refer :all]
            [advent.2020.day18 :as d]))

(deftest puzzle1
  (testing "Examples"
    (is (= 71 (d/eval-xp "1 + 2 * 3 + 4 * 5 + 6" {+ 0 * 0})))
    (is (= 51 (d/eval-xp "1 + (2 * 3) + (4 * (5 + 6))" {+ 0 * 0}))))

  (testing "Actual input"
    (is (= 23507031841020 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 231 (d/eval-xp "1 + 2 * 3 + 4 * 5 + 6" {+ 1 * 0}))))

  (testing "Actual input"
    (is (= 218621700997826 (d/puzzle2 d/puzzle-input)))))
