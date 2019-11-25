(ns advent.2018.day3
  "Advent of Code 2018, day 3: No Matter How You Slice It"
  (:require [advent.helpers :as h]))

; patch id - horizontal offset - vertical offset - width - height
(def row-pattern #"#(\d+) @ (\d+).(\d+): (\d+)x(\d+)")

(def puzzle-input (h/slurp-resource "2018/day3.txt" h/slurp-lines))

(defn- parse-row [row]
  (->> (re-matches row-pattern row) rest (map #(Integer/parseInt %))))

(defn- offset-range [offset v] (range offset (+ offset v)))

(defn rect-range [[_ left-offset top-offset x y]]
  (for [x (offset-range left-offset x) y (offset-range top-offset y)] [x y]))

;;;

(defn puzzle1 [input]
  (->> (map parse-row input)
       (reduce #(merge-with + %1 (zipmap (rect-range %2) (repeat 1))) {})
       (reduce-kv (fn [acc _ v] (if (< 1 v) (inc acc) acc)) 0)))

(defn puzzle2 [input]
  (loop [grid {} non-overlapping #{} rows (map parse-row input)]
    (if (seq rows)
      (let [rng (rect-range (first rows))
            id (ffirst rows)
            overlaps-with (distinct (keep grid rng))]
        (recur
          ; Store the patch ID in the grid value, so we can figure out
          ; if there are overlap conflicts later.
         (merge grid (zipmap rng (repeat id)))
         (if (seq overlaps-with)
           (apply disj non-overlapping (conj overlaps-with id))
           (conj non-overlapping id))
         (rest rows)))
      (first non-overlapping))))
