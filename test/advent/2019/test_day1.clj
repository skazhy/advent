(ns advent.2019.test-day1
  (:require [clojure.test :refer :all]
            [advent.2019.day1 :as d]))

(deftest puzzle1
  (is (= 3408471 (d/puzzle1 d/puzzle-input))))

(deftest puzzle2
  (is (= 5109803 (d/puzzle2 d/puzzle-input))))
