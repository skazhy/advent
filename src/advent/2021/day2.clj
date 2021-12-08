(ns advent.2021.day2
  "Advent of Code 2021, day 2: Dive!"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2021/day2.txt" h/slurp-lines))

(defn parse-row [r] (update (str/split r #" ") 1 h/parse-int))

(defn run-submarine [reduce-step input]
  (->> (map parse-row input)
       (reduce reduce-step [0 0 0])
       (take 2)
       (apply *)))

(defn apply-command [pos [direction amount]]
  (case direction
    "forward" (update pos 0 + amount)
    "up" (update pos 1 - amount)
    "down" (update pos 1 + amount)))

(defn puzzle1 [input] (run-submarine apply-command input))

(defn apply-command-2 [pos [direction amount]]
  (case direction
    "up" (update pos 2 - amount)
    "down" (update pos 2 + amount)
    "forward" (-> (update pos 0 + amount)
                  (update 1 + (* amount (last pos))))))

(defn puzzle2 [input] (run-submarine apply-command-2 input))
