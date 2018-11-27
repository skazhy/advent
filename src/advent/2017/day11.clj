(ns advent.2017.day11
  "Advent of Code 2017, day 11: Hex grids"
  (:require [advent.helpers :as h]))


(def puzzle-input (h/slurp-resource "2017/day11.txt" h/slurp-line))

(defn- parse-input [input] (clojure.string/split input #","))

;;; Coordinate system
;;; all odd x columns are "half step" higher than even x columns.
;;;
;;; < 0  2 >----< 2  2 >----<
;;;  >----< 1  1 >----< 3  1 >
;;; < 0  1 >----< 2  1 >----<
;;;  >----< 1  0 >----< 3  0 >
;;; < 0  0 >----< 2  0 >----<
;;;  >----< 1 -1 >----< 3 -1 >
;;; < 0 -1 >----< 2 -1 >----<

(def ^:private odd-x-direction
  {"n" [0 1] "s" [0 -1] "ne" [1 1] "nw" [-1 1] "sw" [-1 0] "se" [1 0]})

(def ^:private even-x-direction
  {"n" [0 1] "s" [0 -1] "ne" [1 0] "nw" [-1 0] "sw" [-1 -1] "se" [1 -1]})

(defn move [loc direction]
  (if (even? (first loc))
    (get even-x-direction direction)
    (get odd-x-direction direction)))

(defn steps-to-origin [point]
  (let [[x y] (map #(Math/abs %) point)]
    (+ y (if (< 1  x) (int (Math/ceil (/ x 2))) 0))))

(defn navigate-home [steps callback-fn]
  (loop [steps steps loc [0 0] furthest 0]
    (if-let [m (move loc (first steps))]
      (recur (rest steps) (map + loc m) (max furthest (steps-to-origin loc)))
      (callback-fn (steps-to-origin loc)
                    (max furthest (steps-to-origin loc))))))

(defn puzzle1 [input]
  (navigate-home (parse-input input) (fn [step-count _] step-count)))

(defn puzzle2 [input]
  (navigate-home (parse-input input) (fn [_ max-distance] max-distance)))
