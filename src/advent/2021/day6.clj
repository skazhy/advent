(ns advent.2021.day6
  "Advent of Code 2021, day 6: Lanternfish"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2021/day6.txt" h/slurp-int-csv-line))

(defn lifecycle-frequencies [fish]
  (let [init-freqs (frequencies fish)]
    (mapv #(get init-freqs % 0) (range 9))))

(defn run-day [fish-freq]
  (let [updated-freq (conj (vec (rest fish-freq)) (first fish-freq))]
    (update updated-freq 6 + (last updated-freq))))

(defn lanternfish-on-day [input i]
  (apply + (nth (iterate run-day (lifecycle-frequencies input)) i)))

(defn puzzle1 [input] (lanternfish-on-day input 80))
(defn puzzle2 [input] (lanternfish-on-day input 256))
