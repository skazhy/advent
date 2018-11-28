(ns advent.2017.day22
  (:require [advent.helpers.grid :as g]))


(defn infected? [cell] (= \# cell))

(defn gen-grid [rows]
  (->> rows
       (map-indexed
         (fn [idx row]
           (apply concat (map-indexed #(vector [idx %1] (char %2)) row))))
       (apply concat)
       (apply hash-map)))

(defn run-virus [rows iterations transform-fn direction-fn]
  (let [midpoint (int (Math/floor (/ (count (first rows)) 2)))]
    (loop [g (gen-grid rows)
           pos [[midpoint midpoint] g/up-direction]
           direction g/up-direction
           iterations iterations
           infects 0]
      (if (pos? iterations)
        (let [direction (direction-fn (get g coord \.) direction)
              cell-val (transform-fn (get g coord \.))]
          (recur (assoc g cell-val)
                 (mapv + coord direction)
                 direction
                 (dec iterations)
                 (if (infected? cell-val) (inc infects) infects)))
        infects))))

;;; Puzzle 1

(defn puzzle1 [rows iterations]
  (run-virus rows
             iterations
             #(if (infected? %) \. \#)
             #(if (infected? %1) (g/turn-right %2) (g/turn-left %2))))

;;; Puzzle 2

(defn new-direction-2 [cell dir]
  (case cell
    \. (g/turn-left dir)
    \# (g/turn-right dir)
    \W dir
    \F (g/turn-around dir)))

(defn puzzle2 [rows iterations]
  (run-virus rows iterations {\. \W, \W \#, \# \F, \F \.} new-direction-2))
