(ns advent.2020.day1
  "Advent of Code 2020, day 1: Report Repair"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day1.txt" h/slurp-int-lines))

(defn some-result [entry-set year]
  (some #(when (contains? entry-set (- year %)) (* % (- year %))) entry-set))

(defn puzzle1 [input] (some-result (set input) 2020))

(defn puzzle2 [input]
  (let [entry-set (set input)]
    (reduce
     (fn [_ x]
       (when-let [res (some-result entry-set (- 2020 x))]
         (reduced (* res x))))
     input)))
