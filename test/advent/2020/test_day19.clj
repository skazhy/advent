(ns advent.2020.test-day19
  (:require [clojure.test :refer :all]
            [advent.2020.day19 :as d]))

(def ^:private example "test")

(deftest puzzle1
  (testing "Examples"
    (is (= 42 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 42 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 42 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 42 (d/puzzle2 d/puzzle-input)))))
