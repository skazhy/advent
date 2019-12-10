(ns advent.2019.day7
  "Advent of Code 2019, day 7: Amplification Circuit"
  (:require [clojure.core.async :refer [<!! <! >! go] :as async]
            [advent.helpers.intcode :refer [make-input-channel run-program
                                            run-program-async]]
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
            (-> (run-program program [input acc]) :output first))
          0
          inputs))

(defn puzzle1 [program]
  (apply max (map #(run-multiple program %) (permutations [0 1 2 3 4]))))

(defn run-multiple-async [program inputs]
  (let [out (async/chan)]
    (go
      (let [inputs (-> (mapv vector inputs)
                       (update 0 conj 0))
            channels (map make-input-channel inputs)
            programs (map (fn [i o] (run-program-async program i o)) channels
                          (next (cycle channels)))]
        (->> (last programs) <! :output first (>! out))
        (async/close! out)))
    out))

(defn puzzle2 [program]
  (apply max (map #(<!! (run-multiple-async program %))
                  (permutations [5 6 7 8 9]))))
