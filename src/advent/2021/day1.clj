(ns advent.2021.day1
  "Advent of Code 2021, day 1: Sonar Sweep"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2021/day1.txt" h/slurp-int-lines))

(defn increase-count [i] (count (filter true? (map < i (drop 1 i)))))

(def puzzle1 increase-count)

(defn puzzle2 [input]
  (increase-count (map + input (drop 1 input) (drop 2 input))))
