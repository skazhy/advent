(ns advent.2021.test-day6
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2021.day6 :as d]))

(def ^:private example [3 4 3 1 2])

(deftest puzzle1
  (testing "Examples"
    (is (= 5934 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 383160 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 26984457539 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 1721148811504 (d/puzzle2 d/puzzle-input)))))
