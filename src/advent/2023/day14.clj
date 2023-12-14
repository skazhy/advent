(ns advent.2023.day14
  "Advent of Code 2023, day 14: Parabolic Reflector Dish"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2023/day14.txt" h/slurp-lines))

(defn rotate-cw [grid]
  (apply map (comp reverse vector) grid))

(defn rotate-ccw [grid]
  (reverse (apply map vector grid)))

(defn tilt-east [row]
  (mapcat (fn [s] (sort-by #(not= \. %) s)) (partition-by #(= \# %) row)))

(defn tilt-west [row]
  (mapcat (fn [s] (sort-by #(= \. %) s)) (partition-by #(= \# %) row)))

(defn tilt-north [row]
  (->> row rotate-cw (map tilt-east) rotate-ccw))

(defn total-load [grid]
  (loop [grid grid
         i (count grid)
         load 0]
    (if-let [r (first grid)]
      (recur (rest grid)
             (dec i)
             (+ load (* i (count (filter #(= \O %) r)))))
      load)))

;;; Part 1

(defn puzzle1 [grid] (total-load (tilt-north grid)))

;;; Part 2

(defn spin-cycle [grid]
  (->> (tilt-north grid)
       (map tilt-west)
       rotate-ccw (map tilt-east) rotate-cw  ;; tilt south
       (map tilt-east)))

(defn puzzle2 [grid]
  ;; After looking through a handful of outputs, I found that after 100 unique outputs,
  ;; load values start repeating after 14 values.
  (loop [grid grid i (+ 100 (mod (- 1000000000 100) 14))]
    (if (pos? i)
      (recur (spin-cycle grid) (dec i))
      (total-load grid))))
