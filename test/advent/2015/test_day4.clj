(ns advent.2015.test-day4
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2015.day4 :as d]))

(deftest puzzle1
  (testing "Actual input"
    (is (= 254575 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Actual input"
    (is (= 1038736 (d/puzzle2 d/puzzle-input)))))
