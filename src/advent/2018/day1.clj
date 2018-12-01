(ns advent.2018.day1
  "Advent of Code 2018, day 1: Chronal Calibration"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2018/day1.txt" h/slurp-int-lines))

(defn puzzle1 [input]
  (apply + input))

;;; This performs slightly better with reductions used with reduce OR
;;; by using loop and a let-binding to calculate intermediate value.

(defn puzzle2 [input]
  (loop [items (reductions + (cycle input)) seen #{0}]
    (or (seen (first items))
        (recur (drop 1 items) (conj seen (first items))))))
