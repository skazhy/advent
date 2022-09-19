(ns advent.2016.test-day4
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2016.day4 :as d]))

(deftest puzzle1
  (testing "Actual input"
    (is (= 137896 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Actual input"
    (is (= 501 (d/puzzle2 d/puzzle-input)))))
