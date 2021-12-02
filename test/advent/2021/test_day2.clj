(ns advent.2021.test-day2
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2021.day2 :as d]))

(def ^:private  example
  ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"])

(deftest puzzle1
  (testing "Examples"
    (is (= 150 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 2019945 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 900 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 1599311480 (d/puzzle2 d/puzzle-input)))))
