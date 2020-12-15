(ns advent.2020.test-day15
  (:require [clojure.test :refer :all]
            [advent.2020.day15 :as d]))

(def example [0 3 6])

(deftest puzzle1
  (testing "Examples"
    (is (= 436 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 203 (d/puzzle1 d/puzzle-input)))))

(deftest ^:slow puzzle2
  (testing "Examples"
    (is (= 175594 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 9007186 (d/puzzle2 d/puzzle-input)))))
