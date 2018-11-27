(ns advent.2017.test-day8
  (:require [clojure.test :refer :all]
            [advent.helpers :refer [split-lines]]
            [advent.2017.day8 :as d]))


(def ^:private day8-example
  (split-lines
    ["b inc 5 if a > 1"
     "a inc 1 if b < 5"
     "c dec -10 if a >= 1"
     "c inc -20 if c == 10"]))


(deftest puzzle1
  (testing "Example"
    (is (= 1 (d/puzzle1 day8-example))))

  (testing "Actual input"
    (is (= 3745 (d/puzzle1 d/puzzle-input)))))


(deftest puzzle2
  (testing "Example"
    (is (= 10 (d/puzzle2 day8-example))))

  (testing "Actual input"
    (is (= 4644 (d/puzzle2 d/puzzle-input)))))
