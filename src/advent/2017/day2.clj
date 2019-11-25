(ns advent.2017.day2
  "Advent of Code 2017, day 2: Checksums"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource  "2017/day2.txt" h/slurp-int-matrix))

;;; Finding difference of min and max values in a row

(defn- row-extreme-sum [row]
  (- (apply max row) (apply min row)))

(defn puzzle1
  "Corruption checksum"
  [int-matrix]
  (reduce (fn [acc row] (+ acc (row-extreme-sum row))) 0 int-matrix))


;;; Finding the sum of 2 evenly divisible numbers in a row


(defn- evenly-divisible? [a b]
  (when (and (pos? a) (pos? b))
    (if (> a b)
      (when (zero? (mod a b)) (/ a b))
      (when (zero? (mod b a)) (/ b a)))))

(defn- evenly-divisible-row [row]
  (-> (reduce
       (fn [past item]
         (if-let [res (first (keep #(evenly-divisible? % item) past))]
           (reduced {:result res})
           (conj past item)))
       [] row)
      (:result 0)))

(defn puzzle2
  "Evenly divisible checksum"
  [int-matrix]
  (reduce (fn [acc row] (+ acc (evenly-divisible-row row))) 0 int-matrix))

