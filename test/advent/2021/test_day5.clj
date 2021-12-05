(ns advent.2021.test-day5
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2021.day5 :as d]))

(def ^:private example ["0,9 -> 5,9"
                        "8,0 -> 0,8"
                        "9,4 -> 3,4"
                        "2,2 -> 2,1"
                        "7,0 -> 7,4"
                        "6,4 -> 2,0"
                        "0,9 -> 2,9"
                        "3,4 -> 1,4"
                        "0,0 -> 8,8"
                        "5,5 -> 8,2"])

(deftest puzzle1
  (testing "Examples"
    (is (= 5 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 5632 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 12 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 22213 (d/puzzle2 d/puzzle-input)))))
