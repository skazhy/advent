(ns advent.2020.day3
  "Advent of Code 2020, day 3: Toboggan Trajectory"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day3.txt" h/slurp-lines))

(defn traverse-area [area-map incr]
  (loop [coords [0 0] c 0]
    (let [coords (update (mapv + coords incr) 1 mod (count (first area-map)))]
      (if (< (first coords) (count area-map))
        (recur coords (if (= (get-in area-map coords) \#) (inc c) c))
        c))))

(defn puzzle1 [input] (traverse-area input [1 3]))

(defn puzzle2 [input]
  (apply * (map #(traverse-area input %) [[1 1] [1 3] [1 5] [1 7] [2 1]])))
