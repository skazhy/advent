(ns advent.2017.test-day14
  (:require [clojure.test :refer :all]
            [advent.2017.day14 :as d]))


;;; No test examples were given on day 14.

(deftest ^:slow puzzle1
  (is (= 8190 (d/puzzle1 d/puzzle-input))))

(deftest ^:slow puzzle2
  (is (= 1134 (d/puzzle2 d/puzzle-input))))
