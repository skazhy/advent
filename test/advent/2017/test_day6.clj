(ns advent.2017.test-day6
  (:require [clojure.test :refer :all]
            [advent.2017.day6 :as d]))


(deftest puzzle1
  (testing "Example"
    (is (= 5 (d/puzzle1 [0 2 7 0]))))

  (testing "Actual input"
    (is (= 11137 (d/puzzle1 d/puzzle-input)))))


(deftest puzzle2
  (testing "Example"
    (is (= 4 (d/puzzle2 [0 2 7 0]))))

  (testing "Actual input"
    (is (= 1037 (d/puzzle2 d/puzzle-input)))))
