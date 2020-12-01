(ns advent.2020.day1
  "Advent of Code 2020, day 1: Report Repair"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day1.txt" h/slurp-int-lines))

(defn find-matching-entry [entry-set sum]
  (some #(when (contains? entry-set (- sum %)) (* % (- sum %))) entry-set))

(defn puzzle1 [input] (find-matching-entry (set input) 2020))

(defn puzzle2 [input]
  (let [entry-set (set input)]
    (reduce
     (fn [_ x]
       (when-let [res (find-matching-entry entry-set (- 2020 x))]
         (reduced (* res x))))
     input)))
