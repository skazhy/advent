(ns advent.2018.day1
  "Advent of Code 2018, day 1: Chronal Calibration"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2018/day1.txt" h/slurp-int-lines))

(defn puzzle1 [input]
  (apply + input))

(defn puzzle2 [input]
  (loop [items (cycle input) frequency 0 seen #{0}]
    (let [frequency (+ frequency (first items))]
      (if (contains? seen frequency)
        frequency
        (recur (drop 1 items) frequency (conj seen frequency))))))
