(ns advent.2017.test-day5
  (:require [clojure.test :refer :all]
            [advent.2017.day5 :as d]))


(deftest ^:slow puzzle1
  (testing "Example"
    (is (= 5 (d/puzzle1 [0 3 0 1 -3]))))

  (testing "Actual input"
    (is (= 364539 (d/puzzle1 d/puzzle-input)))))


(deftest ^:slow puzzle2
  (testing "Example"
    (= 10 (d/puzzle2 [0 3 0 1 -3])))

  (testing "Actual input"
    (= 27477714 (d/puzzle2 d/puzzle-input))))
