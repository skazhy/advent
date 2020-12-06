(ns advent.2020.day6
  "Advent of Code 2020, day 6: Custom Customs"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day6.txt" h/slurp-lines))

(defn group-items [items]
  (when (seq items)
    (let [[x xs] (split-with #(not= "" %) items)]
      (cons x (group-items (rest xs))))))

(defn puzzle1 [input]
  (->> (group-items input)
       (map (comp count #(reduce into #{} %)) )
       (apply +)))

(defn puzzle2 [input]
  (->> (group-items input)
       (mapcat #(distinct (apply concat %)))
       (count)))
