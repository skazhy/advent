(ns advent.2020.test-day17
  (:require [clojure.test :refer :all]
            [advent.2020.day17 :as d]))

(def example [".#." "..#" "###"])

(deftest puzzle1
  (testing "Examples"
    (is (= 112 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 353 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 848 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 2472 (d/puzzle2 d/puzzle-input)))))
