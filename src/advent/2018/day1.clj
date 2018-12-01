(ns advent.2018.day1
  "Advent of Code 2018, day 1: Chronal Calibration"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2018/day1.txt" h/slurp-int-lines))

(defn puzzle1 [input]
  (apply + input))

(defn puzzle2 [input]
  (reduce
    (fn [[freq acc] item]
      (let [freq (+ freq item)]
        (if (get acc freq)
          (reduced freq)
          [freq (assoc acc freq true)])))
    [0 {0 true}]
    (cycle input)))
