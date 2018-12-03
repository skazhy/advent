(ns advent.2018.day3
  "Advent of Code 2018, day 3: No Matter How You Slice It"
  (:require [advent.helpers :as h]))

(def row-pattern #"#(\d+) @ (\d+).(\d+): (\d+)x(\d+)")

(def puzzle-input (h/slurp-resource "2018/day3.txt" h/slurp-lines))

(defn- parse-row [row]
  (->> (re-matches row-pattern row) rest (map #(Integer. %))))

(defn rect-range [[_ left-offset top-offset x y]]
  (for [x (range left-offset (+ left-offset x))
        y (range top-offset (+ top-offset y))]
    [x y]))

(defn puzzle1 [input]
  (loop [grid {} rows (map parse-row input)]
    (if (seq rows)
      (recur
        ; Store number of overlaps in the grid map value.
        (merge-with + grid (zipmap (rect-range (first rows)) (repeat 1)))
        (rest rows))
      (reduce-kv (fn [acc _ v] (if (< 1 v) (inc acc) acc)) 0 grid))))

(defn puzzle2 [input]
  (loop [grid {} non-overlapping #{} rows (map parse-row input)]
    (if (seq rows)
      (let [rng (rect-range (first rows))
            id (ffirst rows)
            overlaps-with (distinct (keep #(get grid %) rng))]
        (recur
          ; Store the patch ID in the grid value, so we can figure out
          ; if there are overlap conflicts later.
          (merge grid (zipmap rng (repeat id)))
          (if (seq overlaps-with)
            (apply disj non-overlapping (conj overlaps-with id))
            (conj non-overlapping id))
          (rest rows)))
      (first non-overlapping))))
