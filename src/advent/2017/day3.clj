(ns advent.2017.day3
  "Advent of Code 2017, day 3: Taxicab geometry"
  (:require [advent.helpers :as h]
            [advent.helpers.grid :refer [all-neighbors]]))


(def puzzle-input (h/slurp-resource "2017/day3.txt" h/slurp-int))

;;; "Turning" = flipping the grid
;;;
;;; pos(ition) [x y] in the grid
;;; dir(ection) [x y] - the current movement
;;; direction x: -1 left / 0 no horizontal movement / 1 right
;;; direction y: -1 down / 0 no vertical movement / 1 up
;;;
;;; Move from 1 -> 2 is treated like a turn around bottom left corner, so
;;; we always start with downward direction.

(defn- turn-up? [[x y]] (and (pos? x) (= x (- 1 y))))
(defn- turn-down? [[x y]] (and (neg? x) (= x (h/neg y))))
(defn- turn-left? [[x y]] (= x y))

(def origin-pos [0 0])

;;; Bruteforce Manhattan distance calculation between (0,0) and a given point.

(defn- update-direction
  "Updates direction based on the current position."
  [pos dir]
  (if (or (turn-up? pos) (turn-down? pos) (turn-left? pos))
    [(h/neg (second dir)) (first dir)] ; Do a 90o turn by flipping directions
    dir))

(defn puzzle1
  "Manhattan distance"
  [n]
  (let [[pos] (reduce
                (fn [[pos dir] _]
                  (let [dir (update-direction pos dir)]
                    [(mapv + pos dir) dir]))
                [origin-pos [0 -1]]
                (range (dec n)))]
    (apply + (map #(Math/abs %) pos))))

;;; Calculating a taxicab point value


(defn- neighbor-sum
  "Returns the sum of all neighboring cell values."
  [grid [x y]]
  (apply + (map (fn [[nx ny]] (get grid [(+ x nx) (+ y ny)] 0)) all-neighbors)))

(defn puzzle2
  "Neighbor sums"
  [n]
  (loop [pos origin-pos dir [0 -1] grid {}]
    (let [cell-val (neighbor-sum grid pos)
          dir (update-direction pos dir)
          grid (if (= origin-pos pos)
                     ; Handle empty grids correctly.
                     (assoc grid pos 1)
                     (assoc grid pos cell-val))]
      (if (< n cell-val)
        cell-val
        (recur (mapv + pos dir) dir grid)))))
