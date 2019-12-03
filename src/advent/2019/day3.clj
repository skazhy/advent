(ns advent.2019.day3
  "Advent of Code 2019, day 3: Crossed Wires"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2019/day3.txt" h/slurp-csv-lines))

(defn parse-wire-input [s]
  [(first s) (Integer/parseInt (.substring s 1))])

(def directions
  {\L [-1 0]
   \R [1 0]
   \U [0 1]
   \D [0 -1]})

;; wire is a vector containing all coordinates a wire passes
;; eg: [ [1 0] [2 0] [2 1] ...]

(defn mk-wire [input]
  (loop [input input wire [(list 0 0)]]
    (if-let [[dir len] (some-> (first input) parse-wire-input)]
      (let [direction (get directions dir)]
        (recur
         (rest input)
         (->> (range len)
              (reductions (fn [acc _] (map + acc direction)) (last wire))
              (rest)  ; remove duplications on wire turns
              (into wire))))
      (rest wire))))  ; remove the initial [0 0] coordinate

(defn manhattan-distance
  "Returns Manhattan distance to a given coordinate"
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn minimal-by
  "Finds the minimal mapped value for the collection."
  [cmp [h & tail]]
  (reduce (fn [acc i] (min acc (cmp i))) (cmp h) tail))

(defn puzzle1
  "Returns the closest intersection for two given wires"
  [[input-a input-b]]
  ; Drop [0 0] since it doesn't count as an intersection.
  (->> (h/intersection (rest (mk-wire input-a)) (rest (mk-wire input-b)))
       (minimal-by manhattan-distance)))

(defn puzzle2 [[input-a input-b]]
  (let [wire-a (mk-wire input-a)
        wire-b (mk-wire input-b)
        ; + 1 for each removed [0 0] coordinate
        combined-distance #(+ 2 (.indexOf wire-a %) (.indexOf wire-b %))]
    (minimal-by combined-distance (h/intersection wire-a wire-b))))
