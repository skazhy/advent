(ns advent.2017.day17
  "Advent of Code 2017, day 17: Spinlock"
  (:require [advent.helpers :as h]))


(def puzzle-input (h/slurp-resource "2017/day17.txt" h/slurp-int))

(defn- insert-after [pos steps cnt]
  (mod (+ pos steps) cnt))


;;; Puzzle 1

(defn- iterate-over [steps cnt]
  (loop [pos 0 lst [0]]
    (if (<= (count lst) cnt)
      (let [idx (insert-after pos steps (count lst))]
        (recur (inc idx)
               (-> (take (inc idx) lst) vec
                   (conj (count lst))
                   (into (drop (inc idx) lst)))))
      lst)))

(defn puzzle1 [steps]
  (let [res (iterate-over steps 2017)]
    (nth res (inc (.indexOf res 2017)))))


;;; Puzzle 2

(defn- faux-iterate-over [steps cnt]
  (loop [pos 0 len 1 after-0 nil]
    (if (< len cnt)
      (let [idx (insert-after pos steps len)]
        (recur (inc idx) (inc len) (if (zero? idx) len after-0)))
      after-0)))

(defn puzzle2 [steps]
  (faux-iterate-over steps 50000000))
