(ns advent.2020.test-day16
  (:require [clojure.test :refer :all]
            [advent.2020.day16 :as d]))

(def ^:private example ["class: 1-3 or 5-7"
              "row: 6-11 or 33-44"
              "seat: 13-40 or 45-50"
              ""
              "your ticket:"
              "7,1,14"
              ""
              "nearby tickets:"
              "7,3,47"
              "40,4,50"
              "55,2,20"
              "38,6,12"])

(deftest puzzle1
  (testing "Examples"
    (is (= 71 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 20048 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Actual input"
    (is (= 4810284647569 (d/puzzle2 d/puzzle-input)))))
