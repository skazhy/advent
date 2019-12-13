(ns advent.2019.day13
  "Advent of Code 2019, day 13: Care Package"
  (:require [clojure.core.async :refer [<!! <! go-loop] :as async]
            [advent.helpers.intcode :refer [run-program-async]]
            [advent.helpers :as h]))

(def puzzle-input
  (h/slurp-resource "2019/day13.txt" (comp vec h/slurp-int-csv-line)))

(defn puzzle1 [program]
  (let [output-chan (async/chan)]
    (run-program-async program (async/chan) output-chan)
    (<!!
     (go-loop [out []]
       (if-let [v (<! output-chan)]
         (recur (conj out v))
         (count (filter #(= 2 (last %)) (partition 3 out))))))))
