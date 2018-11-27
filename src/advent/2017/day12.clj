(ns advent.2017.day12
  "Advent of Code 2017, day 12: dependency graphs"
  (:require [advent.helpers :as h]))


(def puzzle-input (h/slurp-resource "2017/day12.txt" h/slurp-lines))

(defn- parse-row [row]
  (let [[p _ & connected] (clojure.string/split row #"[\s,]+")]
    [p (set connected)]))

(defn- parse-rows [rows]
  (apply hash-map (mapcat parse-row rows)))

(defn group-members [conns init-m]
  (loop [to-check #{init-m} seen #{} conns conns]
    (if-let [new-s (seq (mapcat #(get conns %) to-check))]
      (recur new-s (into seen to-check) (apply dissoc conns to-check))
      seen)))


;;; Puzzle 1

(defn puzzle1 [input]
  (let [conns (parse-rows input)]
    (count (group-members conns "0"))))


;;; Puzzle 2

(defn- group-count [conns]
  (loop [conns conns init-m "0" group-count 1]
    (let [conn-subset (apply dissoc conns (group-members conns init-m))]
      (if (empty? conn-subset)
        group-count
        (recur conn-subset (ffirst conn-subset) (inc group-count))))))

(defn puzzle2 [input]
  (let [conns (parse-rows input)]
    (group-count conns)))
