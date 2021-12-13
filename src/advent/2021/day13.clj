(ns advent.2021.day13
  "Advent of Code 2021, day 13: Transparent Origami"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2021/day13.txt" h/slurp-lines))

(defn parse-line [l]
  (re-matches #"^(\d+),(\d+)|fold along ([xy])=(\d+)$" l))

(defn init-state [coords folds]
  (let [bounds (reduce (fn [acc [xy a]] (update acc xy (fnil max 0) a)) {} folds)]
    (reduce (fn [acc c] (assoc-in acc (reverse c) true))
            (vec (repeat (inc (* 2 (get bounds "y")))
                         (vec (repeat (inc (* 2 (get bounds "x"))) false))))
            coords)))

(defn merge-subrows [a b] (map #(or %1 %2) a b))

(defn fold-up [grid i]
  (mapv merge-subrows (take i grid) (reverse (drop (inc i) grid))))

(defn fold-left [grid i]
  (mapv #(merge-subrows (take i %) (reverse (drop (inc i) %))) grid))

(defn run-folds [input take-folds]
  (let [[coords folds] (split-with (comp nil? last) (keep parse-line input))
        folds (map (fn [[_ _ _ xy a]] [xy (h/parse-int a)]) folds)
        coords (map (fn [[_ x y]] [(h/parse-int x) (h/parse-int y)]) coords)]
    (reduce (fn [grid [xy a]]
              (case xy
                "x" (fold-left grid a)
                "y" (fold-up grid a)))
            (init-state coords folds)
            (take-folds folds))))

(defn puzzle1 [input]
  (count (filter identity (flatten (run-folds input #(take 1 %))))))

(defn puzzle2 [input]
  (map (fn [row] (println (apply str (map #(if % "#" ".") row))))
       (run-folds input identity)))
