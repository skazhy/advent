(ns advent.2023.day9
  "Advent of Code 2023, day 9: Mirage Maintenance"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2023/day9.txt" h/slurp-int-matrix))

(defn extrapolate [row reducer]
  (->> (iterate #(map - (next %) %) row)
       (take-while #(not (every? zero? %)))
       reverse
       (reduce reducer 0)))

(defn extraploation-sum [rows reducer]
  (apply + (map #(extrapolate % reducer) rows)))

(defn puzzle1 [input] (extraploation-sum input (fn [last-item row] (+ (last row) last-item))))
(defn puzzle2 [input] (extraploation-sum input (fn [last-item row] (- (first row) last-item))))
