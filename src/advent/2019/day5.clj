(ns advent.2019.day5
  "Advent of Code 2019, day 5: Sunny with a Chance of Asteroids"
  (:require [advent.helpers :as h]
            [advent.helpers.intcode :refer [run-program]]))

(def puzzle-input
  (h/slurp-resource "2019/day5.txt" (comp vec h/slurp-int-csv-line)))

;;; All interesting bits for this puzzle are in the incode implementation.

(defn last-program-output [program input]
  (-> (run-program program [input]) :output last))

(defn puzzle1 [program] (last-program-output program 1))

(defn puzzle2 [program] (last-program-output program 5))
