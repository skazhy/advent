(ns advent.2024.day1
  "Advent of Code 2024, day 1: Historian Hysteria"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2024/day1.txt" h/slurp-int-matrix))

(defn puzzle1 [input]
  (let [[a b] (reduce (fn [[acc-a acc-b] [a b]]
                        [(conj acc-a a) (conj acc-b b)])
                      [[] []]
                      input)]
    (apply + (map #(Math/abs (- %1 %2)) (sort a) (sort b)))))

(defn puzzle2 [input]
  (let [[a b] (reduce (fn [[acc-a acc-b] [a b]]
                        [(conj acc-a a) (update acc-b b (fnil inc 0))])
                      [[] {}]
                      input)]
    (apply + (map #(* % (get b % 0)) a))))
