(ns advent.2021.day5
  "Advent of Code 2021, day 5: Day 5: Hydrothermal Venture"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2021/day5.txt" h/slurp-lines))

(defn parse-coord [x] (map h/parse-int (str/split x #",")))

(defn make-line [[[x0 y0] [x1 y1]] allow-diagonals?]
  (let [x-range (range (min x0 x1) (inc (max x0 x1)))
        y-range (range (min y0 y1) (inc (max y0 y1)))]
    (cond
      (= x0 x1) (map #(vector x0 %) y-range)
      (= y0 y1) (map #(vector % y0) x-range)
      allow-diagonals? (map vector
                            (if (< y1 y0) (reverse x-range) x-range)
                            (if (< x1 x0) (reverse y-range) y-range)))))

(defn count-dangerous-points [input allow-diagonals?]
  (->> input
       (keep (comp #(make-line % allow-diagonals?)
                   (juxt (comp parse-coord first) (comp parse-coord last))
                   #(str/split % #" ")))
       (apply concat)
       (reduce (fn [acc i] (update acc i (fnil inc 0))) {})
       (filter #(< 1 (val %)))
       (count)))

(defn puzzle1 [input] (count-dangerous-points input false))

(defn puzzle2 [input] (count-dangerous-points input true))
