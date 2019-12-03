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

(defn expanded-wire-intersections [expanded-wire-a expanded-wire-b]
  (set/intersection (set expanded-wire-a) (set expanded-wire-b)))

(defn minimal-by [cmp [h & tail]]
  "Finds the minimal mapped value for the collection."
  (reduce (fn [acc i] (min acc (cmp i))) (cmp h) tail))

(defn puzzle1
  "Returns the closest intersection for two given wires"
  [[wire-a wire-b]]
  (->> (expanded-wire-intersections (expand-wire wire-a) (expand-wire wire-b))
       (minimal-by manhattan-distance)))

(defn puzzle2 [[wire-a wire-b]]
  (let [expanded-a (expand-wire wire-a)
        expanded-b (expand-wire wire-b)]
    (->> (expanded-wire-intersections (set expanded-a)
                                      (set expanded-b))
         ; + 1 for each initial move, since [0 0] was dropped in expand-wire
         (minimal-by #(+ 2 (.indexOf expanded-a %) (.indexOf expanded-b %))))))
