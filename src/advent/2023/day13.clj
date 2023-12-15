(ns advent.2023.day13
  "Advent of Code 2023, day 13: Point of Incidence"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2023/day13.txt" h/slurp-lines))

(defn find-index [rows idx-fn]
  (or (idx-fn rows)
      (when-let [idx (idx-fn (map reverse rows))]
        (- (count (first rows)) idx))
      (let [rotated (apply map (comp reverse vector) rows)]
        (if-let [res (idx-fn rotated)]
          (* 100 (- (count rows) res))
          (* 100 (idx-fn (map reverse rotated)))))))

;;; Part 1
;;; Using full index

(defn full-idx [rows]
  (loop [len (int (/ (count (first rows)) 2))]
    (when (pos? len)
      (if (every? #(= (reverse (take len %)) (take len (drop len %))) rows)
        len
        (recur (dec len))))))

(defn puzzle1 [input]
  (let [[block tail] (split-with #(not= "" %) input)]
    (if (seq tail)
      (+ (puzzle1 (rest tail)) (find-index block full-idx))
      (find-index block full-idx))))

;;; Part 2
;;; Partial reflection

(defn diff-len [a b]
  (count (filter identity (map not= a b))))

(defn partial-idx [rows]
  (loop [len (int (/ (count (first rows)) 2))]
    (when (pos? len)
      (if (->> (map #(diff-len (reverse (take len %)) (take len (drop len %))) rows)
               (apply +)
               (= 1))
        len
        (recur (dec len))))))

(defn puzzle2 [input]
  (let [[block tail] (split-with #(not= "" %) input)]
    (if (seq tail)
      (+ (puzzle2 (rest tail)) (find-index block partial-idx))
      (find-index block partial-idx))))
