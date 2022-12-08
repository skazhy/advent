(ns advent.2022.test-day8
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2022.day8 :as d]))

(def example [[3 0 3 7 3] [2 5 5 1 2] [6 5 3 3 2] [3 3 5 4 9] [3 5 3 9 0]])

(deftest puzzle1
  (testing "Examples"
    (is (= 21 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 1803 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 8 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 268912 (d/puzzle2 d/puzzle-input)))))
