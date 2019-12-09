(ns advent.2019.test-day2
  (:require [clojure.test :refer :all]
            [advent.2019.day2 :as d]))

(deftest ^:intcode puzzle1
  (is (= 3085697 (d/puzzle1 d/puzzle-input))))

(deftest ^:intcode puzzle2
  (is (= 9425 (d/puzzle2 d/puzzle-input))))
