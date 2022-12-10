(ns advent.2022.test-day10
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2022.day10 :as d]))

(deftest puzzle1
  (testing "Actual input"
    (is (= 14040 (d/puzzle1 d/puzzle-input)))))
