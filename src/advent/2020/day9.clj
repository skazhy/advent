(ns advent.2020.day9
  "Advent of Code 2020, day 9: Encoding Error"
  (:require [advent.helpers :as h]))

(def puzzle-input
  (h/slurp-resource "2020/day9.txt" (comp #(map long %) h/slurp-int-lines)))

;;; Preamble contains pairs of numbers & set of sums with all following
;;; elements in the preamble.
;;; [[35 #{60 55 50 82}] [20 #{35 45 67}] [15 #{62 40}] [25 #{72}] [47 #{}]]
(defn preamble-sums [preamble]
  (vec
   (reduce (fn [acc item]
             (conj acc [item (set (map #(+ item (first %)) acc))]))
           '()
           (reverse preamble))))

(defn update-preamble [preamble i]
  (as-> (rest preamble) pre
    (mapv (fn [[p s]] [p (conj s (+ p i))]) pre)
    (conj pre [i #{}])))

(defn find-missing [input pl]
  (loop [preamble (preamble-sums (take pl input)) input (drop pl input)]
    (when-let [i (first input)]
      (if (some #(contains? (second %) i) preamble)
        (recur (update-preamble preamble i) (rest input))
        i))))

(defn puzzle1 [input] (find-missing input 25))

(defn break-encryption [input sum]
  (loop [input input s []]
    (when-let [i (first input)]
      (case (compare (apply + i s) sum)
        -1 (recur (rest input) (conj s i))
        0 (let [s (conj s i)] (+ (apply min s) (apply max s)))
        (recur input (vec (rest s)))))))

(defn puzzle2 [input]
  (break-encryption input (puzzle1 input)))
