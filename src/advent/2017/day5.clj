(ns advent.2017.day5
  "Advent of Code 2017, day 5: Sequence jumps"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2017/day5.txt" h/slurp-int-lines))

(defn- jump [mapper items pos]
  (let [v (+ pos (get items pos))]
    (when (get items v)
      [(update items pos mapper) v])))

(defn- jump-count
  "Given a sequence of jumps and a mapping function, count the max possible
   jumps, starting from the first instruction."
  [mapper items]
  (loop [items (vec items) pos 0 n 1]
    (if-let [[items pos] (jump mapper items pos)]
      (recur items pos (inc n))
      n)))

(defn puzzle1
  "Jumps with increasing offsets"
  [items]
  (jump-count inc items))

(defn puzzle2
  "Jumps with decreasing large offsets"
  [items]
  (jump-count #(if (<= 3 %) (dec %) (inc %)) items))
