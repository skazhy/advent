(ns advent.2022.day8
  "Advent of Code 2022, day 8: Treetop Tree House"
  (:require [advent.helpers :as h]))

(defn parse [input] (mapv (fn [row] (mapv #(- (int %) 48) row)) input))

(def puzzle-input (h/slurp-resource "2022/day8.txt" (comp parse h/slurp-lines)))

;;; Part 1: Finding how many items are "visible from outside of the matrix"

(defn visible-in-seq [indexed-row]
  (loop [row indexed-row max-height -1 visible []]
    (if-let [[i t] (first row)]
      (if (<= t max-height)
        (recur (rest row) max-height visible)
        (recur (rest row) t (conj visible i)))
      visible)))

(defn visible-indexes [input]
  (let [indexed (map-indexed vector input)]
    (distinct (concat (visible-in-seq indexed)
                      (visible-in-seq (reverse indexed))))))

(defn visible-in-rows [input]
  (->> input
       (map-indexed (fn [x row] (map #(vector x %) (visible-indexes row))))
       (apply concat)))

(defn visible-in-cols [input]
  (->> (range (count (first input)))
       (map (fn [y]
              (map #(vector % y)
                   (visible-indexes
                    (map #(nth % y) input)))))
       (apply concat)))

(defn puzzle1 [input]
    (count (distinct (concat (visible-in-rows input) (visible-in-cols input)))))

;;; Part 2: finding item in matrix with

(defn visibility-in-dir [input coords delta]
  (let [max-vis (get-in input coords)]
    (loop [distance 0 coords (map + coords delta)]
      (if (nil? (get-in input coords))
        distance
        (if (<= max-vis (get-in input coords))
          (inc distance)
          (recur (inc distance) (map + coords delta)))))))

(defn scenic-score [input coords]
  (* (visibility-in-dir input coords [0 1])
     (visibility-in-dir input coords [0 -1])
     (visibility-in-dir input coords [1 0])
     (visibility-in-dir input coords [-1 0])))

(defn puzzle2 [input]
  (->> input
       (map-indexed
        (fn [y row]
          (map (fn [x] (scenic-score input [x y])) (range (count row)))))
       (apply concat)
       (apply max)))
