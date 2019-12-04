(ns advent.2019.test-day4
  (:require [clojure.test :refer :all]
            [advent.2019.day4 :as d]))

(deftest puzzle1
  (is (= 2081 (d/puzzle1 d/puzzle-input))))

(deftest puzzle2
  (is (= 1411 (d/puzzle2 d/puzzle-input))))
