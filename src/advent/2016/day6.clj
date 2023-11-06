(ns advent.2016.day6
  "Advent of Code 2016, day 6: Signals and Noise"
  (:require [advent.helpers :as h]
            [clojure.string :as str]))

(def puzzle-input (h/slurp-resource "2016/day6.txt" h/slurp-lines))

(defn key-by [m cmp-fn]
  (first
   (reduce (fn [acc [k v]] (if (cmp-fn (last acc) v) acc [k v])) m)))

(defn combine-messages
  "Combines the messages with the given comparator fn"
  [messages cmp-fn]
  (->> messages
       (reduce (partial map #(update %1 %2 (fnil inc 0))) (repeat {}))
       (map #(key-by % cmp-fn))
       (apply str)))

(defn puzzle1 [input] (combine-messages input >))
(defn puzzle2 [input] (combine-messages input <))
