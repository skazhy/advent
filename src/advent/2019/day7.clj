(ns advent.2019.day7
  "Advent of Code 2019, day 7: Amplification Circuit"
  (:require [clojure.string :as str]
            [advent.helpers.intcode :refer [run-program]]
            [advent.helpers :as h]))

(def puzzle-input
  (h/slurp-resource "2019/day7.txt" (comp vec h/slurp-int-csv-line)))

(defn permutations [coll]
  (if (= 1 (count coll))
    (list coll)
    (for [head coll
          tail (permutations (disj (set coll) head))]
      (cons head tail))))

(defn run-multiple [program inputs]
  (reduce (fn [acc input]
            (-> (run-program program [input acc]) :outputs first))
          0
          inputs))

(defn puzzle1 [program]
  (apply max (map #(run-multiple program %) (permutations [0 1 2 3 4]))))

; (defn puzzle2 [_])
