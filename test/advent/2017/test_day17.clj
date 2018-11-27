(ns advent.2017.test-day17
  (:require [clojure.test :refer :all]
            [advent.2017.day17 :as d]))


(deftest puzzle1
  (testing "Actual input"
    (is (= 1311 (d/puzzle1 d/puzzle-input)))))


(deftest puzzle2
  (testing "Actual input"
    (is (= 39170601 (d/puzzle2 d/puzzle-input)))))
