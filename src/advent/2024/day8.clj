(ns advent.2024.day8
  "Advent of Code 2024, day 8: Resonant Collinearity"
  (:require [advent.helpers :as h]
            [clojure.math.combinatorics :refer [combinations]]))

(def puzzle-input (h/slurp-resource "2024/day8.txt" h/slurp-lines))

(defn antinode-seq
  "Creates a lazy seq with antinodes in one direction from a pair of antennas"
  ([[y x] y-fn x-fn]
   (lazy-seq
    (let [p [(y-fn y) (x-fn x)]]
      (cons p (antinode-seq p y-fn x-fn))))))

(defn antinodes
  "Returns antinodes for agiven pair of antennas, with op applied to results"
  [in-bounds? op [p1 p2]]
  (let [[[min-y x1] [max-y x2]] (sort-by first [p1 p2])
        dy (- max-y min-y)
        dx (Math/abs (- x1 x2))
        x1-op (if (< x1 x2) - +)
        x2-op (if (< x1 x2) + -)]
    (concat (op (take-while in-bounds? (antinode-seq [min-y x1] #(- % dy) #(x1-op % dx))))
            (op (take-while in-bounds? (antinode-seq [max-y x2] #(+ % dy) #(x2-op % dx)))))))

(defn antenna-coords [grid-rows]
  (reduce (fn [acc [y row]]
            (reduce (fn [acc [x cell]]
                      (if (= \. cell)
                        acc
                        (update acc cell conj [y x])))
                    acc
                    (map-indexed vector row)))
          {}
          (map-indexed vector grid-rows)))

(defn in-bounds? [grid-rows]
  (let [y-bound (dec (count grid-rows))
        x-bound (dec (count (first grid-rows)))]
    (fn [[y x]]
      (and (<= 0 y y-bound) (<= 0 x x-bound)))))

(defn unique-antinodes [input filter-op]
  (let [in-bounds? (in-bounds? input)]
    (reduce-kv (fn [acc _ v]
                 (into acc (mapcat (partial antinodes in-bounds? filter-op) (combinations v 2))))
               #{}
               (antenna-coords input))))

(defn puzzle1 [input] (count (unique-antinodes input #(take 1 %))))

(defn puzzle2 [input]
  (->> (antenna-coords input)
       (vals)
       (filter #(< 1 (count %)))
       (apply concat)
       (into (unique-antinodes input identity))
       (count)))
