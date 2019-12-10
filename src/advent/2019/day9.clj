(ns advent.2019.day9
  "Advent of Code 2019, day 9: Sensor Boost"
  (:require [advent.helpers.intcode :refer [run-program]]
            [advent.helpers :as h]))

(def puzzle-input
  (h/slurp-resource "2019/day9.txt" (comp vec h/slurp-int-csv-line)))

(defn puzzle1 [program] (-> (run-program program [1]) :output first))

(defn puzzle1 [program] (-> (run-program program [2]) :output first))
