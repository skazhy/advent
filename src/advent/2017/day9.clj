(ns advent.2017.day9
  "Advent of Code 2017, day 9: Stream parsing"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2017/day9.txt" h/slurp-line))

(defn c-gr [items]
  (loop [items items
         state {:in-garbage? false
                :skip? false
                :depth 0
                :group-count 0
                :garbage-size 0}]
    (if-let [i (first items)]
      (recur
       (rest items)
       (cond
         (:skip? state) (assoc state :skip? false)
         (= \! i) (assoc state :skip? true)
         (and (= \> i) (:in-garbage? state)) (assoc state :in-garbage? false)
         (:in-garbage? state) (update state :garbage-size inc)
         (= \{ i) (update state :depth inc)
         (= \} i) (-> (update state :depth dec)
                      (update :group-count + (:depth state)))
         (= \< i) (assoc state :in-garbage? true)
         :else state))
      state)))

(defn puzzle1 [items] (:group-count (c-gr items)))

(defn puzzle2 [items] (:garbage-size (c-gr items)))
