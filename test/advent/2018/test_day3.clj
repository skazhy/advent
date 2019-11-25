(ns advent.2018.test-day3
  (:require [clojure.test :refer :all]
            [advent.2018.day3 :as d]))

(def ^:private example-input
  ["#1 @ 1.3: 4x4" "#2 @ 3.1: 4x4" "#3 @ 5.5: 2x2"])

(deftest puzzle1
  (testing "Example"
    (is (= 4 (d/puzzle1 example-input))))

  (testing "Actual input"
    (is (= 117505 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Example"
    (is (= 3 (d/puzzle2 example-input))))

  (testing "Actual input"
    (is (= 1254 (d/puzzle2 d/puzzle-input)))))
