(ns advent.2017.test-day15
  (:require [clojure.test :refer :all]
            [advent.2017.day15 :as d]))

(deftest ^:slow puzzle1
  (testing "Example"
    (is (= 588 (d/puzzle1 65 8921))))

  (testing "Actual input"
    (is (= 612 (d/puzzle1 722 354)))))

(deftest ^:slow puzzle2
  (testing "Example"
    (is (= 309 (d/puzzle2 65 8921))))

  (testing "Actual input"
    (is (= 285 (d/puzzle2 722 354)))))
