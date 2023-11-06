(ns advent.2017.test-day21
  (:require [clojure.test :refer :all]
            [advent.2017.day21 :as d]))

(deftest puzzle1
  (testing "Actual input"
    (is (= 197 (d/puzzle1 d/puzzle-input)))))

(deftest ^:slow puzzle2
  (testing "Actual input"
    (is (= 3081737 (d/puzzle2 d/puzzle-input)))))
