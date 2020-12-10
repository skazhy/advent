(ns advent.2020.day10
  "Advent of Code 2020, day 10: Adapter Array"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day10.txt" h/slurp-int-lines))

(defn prepare-input [input] (sort (conj input 0)))

(defn puzzle1 [input]
  (loop [input (prepare-input input) deltas []]
    (if (< 1 (count input))
      (let [[joltage i] input] (recur (rest input) (conj deltas (- i joltage))))
      ;; Add the 1 +3 delta for the final path which is not in input.
      (->> (reduce #(update %1 %2 (fnil inc 0)) {3 1} deltas)
           (vals)
           (apply *)))))

(defn available-paths
  "Go through previous elements & find how many new paths are available to
   get to n-th element."
  [input paths n]
  (reduce
   (fn [acc i]
     (if (< (nth input n) (+ 4 (nth input i)))
       (+ acc (nth paths i))
       acc))
   0
   (range (max 0 (- n 3)) n)))

(defn puzzle2 [input]
  (let [input (prepare-input input)]
    (last
     (reduce
      (fn [paths i] (conj paths (available-paths input paths i)))
      [1]
      (range 1 (count input))))))
