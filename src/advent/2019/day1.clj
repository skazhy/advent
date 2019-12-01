(ns advent.2019.day1
  "Advent of Code 2019, day 1: The Tyranny of the Rocket Equation"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2019/day1.txt" h/slurp-int-lines))

(defn mass-of-module [module]
  (-> (/ module 3)
      (Math/floor)
      (- 2)
      (int)))

(defn combined-mass-of-module [module]
  (loop [tot 0 module module]
    (let [m (mass-of-module module)]
      (if (pos? m)
        (recur (+ tot m) m)
        tot))))

(defn puzzle1 [input]
  (apply + (map mass-of-module input)))

(defn puzzle2 [input]
  (apply + (map combined-mass-of-module input)))
