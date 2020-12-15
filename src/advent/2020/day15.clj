(ns advent.2020.day15
  "Advent of Code 2020, day 15: Rambunctious Recitation"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day15.txt" h/slurp-int-csv-line))

(defn prepare-input [input]
  (zipmap input (map list (range 1 (inc (count input))))))

(defn run-game [input last-turn]
  (loop [turns (prepare-input input) prev-turn (last input) n (inc (count input))]
    (if (<= n last-turn)
      (let [prev-ns (get turns prev-turn)
            new-turn (if (= 1 (count prev-ns)) 0 (apply - prev-ns))]
        (recur (update turns new-turn #(if % (list n (first %)) (list n)))
               new-turn
               (inc n)))
      prev-turn)))

(defn puzzle1 [input] (run-game input 2020))

(defn puzzle2 [input] (run-game input 30000000))
