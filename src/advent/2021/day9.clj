(ns advent.2021.day9
  "Advent of Code 2021, day 9: Smoke Basin"
  (:require [advent.helpers.grid :as grid]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2021/day9.txt" h/slurp-lines))

(defn gen-grid [input]
  (into {}
        (for [x (range (count input)) y (range (count (first input)))]
          [[x y] (h/parse-int (str (get-in input [x y])))])))

(defn neighbor-coords [grid coords]
  (keep #(let [c (mapv + % coords)] (when (get grid c) c)) grid/edge-neighbors))

(defn low-point? [grid [coords v]]
  (every? #(< v %) (map #(get grid %) (neighbor-coords grid coords))))

(defn low-points [grid] (filter #(low-point? grid %) grid))

(defn basin-size-for-low-point [grid coords]
  (loop [unchecked [coords]
         members #{}]
    (if-let [c (first unchecked)]
      (let [new-neighbors (remove #(or (= 9 (get grid %))
                                       (contains? members %))
                                  (neighbor-coords grid c))]
        (recur (into (rest unchecked) new-neighbors)
               (into members new-neighbors)))
      (count members))))

(defn puzzle1 [input]
  (->> (gen-grid input)
       (low-points)
       (map (comp inc last))
       (apply +)))

(defn puzzle2 [input]
  (let [grid (gen-grid input)]
    (->> (low-points grid) keys
         (map #(basin-size-for-low-point grid %))
         (sort)
         (reverse)
         (take 3)
         (apply *))))
