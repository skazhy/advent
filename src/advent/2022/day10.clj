(ns advent.2022.day10
  "Advent of Code 2022, day 10: Cathode-Ray Tube"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2022/day10.txt" h/slurp-lines))

(defn parse [row]
  (let [[_ v] (str/split (str/trim row) #"\s")]
    (cond-> [0]
      v (conj (h/parse-int v)))))

(defn puzzle1 [input]
  (let [addx-deltas (into [1] (mapcat parse input))]
    (reduce (fn [acc n]
              (+ acc (* n (apply + (take n addx-deltas)))))
            0
            [20 60 100 140 180 220])))
;;;

(defn puzzle2 [input]
  (->> (into [1] (mapcat parse input))
       (partition 40)
       (reduce (fn [[sprite rows-out] addx-deltas]
                 (let [[sprite row]
                       (reduce (fn [[sprite row] [idx c]]
                                 (let [sprite (+ sprite c)]
                                   [sprite (conj row (if (<= (dec sprite) idx (inc sprite)) "#" "."))]))
                               [sprite, []]
                               (map-indexed vector addx-deltas))]
                   [sprite (conj rows-out (str/join row))]))
               [0, []])))
