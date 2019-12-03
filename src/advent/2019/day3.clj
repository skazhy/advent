(ns advent.2019.day3
  "Advent of Code 2019, day 3: Crossed Wires"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2019/day3.txt" h/slurp-csv-lines))

(defn parse-wire-segment [s]
  [(first s) (Integer/parseInt (.substring s 1))])

(def directions
  {\L [-1 0]
   \R [1 0]
   \U [0 1]
   \D [0 -1]})

(defn expand-wire [wire]
  (loop [wire wire expanded [(list 0 0)]]
    (if-let [[dir len] (some-> (first wire) parse-wire-segment)]
      (let [move (get directions dir)]
        (recur
          (rest wire)
          (->> (range len)
               (reductions (fn [acc _] (map + acc move)) (last expanded))
               (rest)  ; remove duplications on wire turns
               (into expanded))))
      (rest expanded))))  ; remove [0 0] coordinate

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn puzzle1
  "Returns the closest intersection for two given wires"
  [[wire-a wire-b]]
  (->> (set/intersection (set (expand-wire wire-a)) (set (expand-wire wire-b)))
       (map manhattan-distance)
       (sort)
       (first)))

(defn puzzle2 [input]

  )
