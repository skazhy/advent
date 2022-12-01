(ns advent.2022.day1
  "Advent of Code 2022, day 1: Calorie Counting"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2022/day1.txt" h/slurp-lines))

(defn parse-int [s] (if (empty? s) 0 (h/parse-int s)))

(def parse-xf
  (comp (partition-by #(not= "" %))
        (map #(apply + (map parse-int %)))))

(defn puzzle1 [input]
  (transduce parse-xf max 0 input))

(defn puzzle2 [input]
  (->> (eduction parse-xf input) (sort >) (take 3) (apply +)))
