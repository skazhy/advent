(ns advent.2019.day5
  "Advent of Code 2019, day 5: Sunny with a Chance of Asteroids"
  (:require [advent.helpers :as h]
            [advent.helpers.intcode :refer [run-program]]))

(def puzzle-input
  (h/slurp-resource "2019/day5.txt" (comp vec h/slurp-int-csv-line)))

;;; All interesting bits for this puzzle are in the incode implementation.

(defn puzzle1 [program]
  (-> (run-program program 1) :outputs last))

(defn puzzle2 [program]
  (-> (run-program program 5) :outputs last))
