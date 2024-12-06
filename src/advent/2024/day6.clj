(ns advent.2024.day6
  "Advent of Code 2024, day 6: Guard Gallivant"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2024/day6.txt" h/slurp-lines))

(defn rotate-dir [[y x]] [x (* -1 y)])

(defn move [grid pos]
  (loop [pos pos dir [-1 0] visited {}]
    (if-let [p (get-in grid (mapv + pos dir))]
      (cond
        ;; Already visited cell in given direction - given grid cannot be exited.
        (contains? (get visited pos) dir) nil
        ;; Obstacle ahead - turn 90 degrees and continue
        (= p \#) (recur pos (rotate-dir dir) (update visited pos (fnil conj #{}) dir))
        ;; Move 1 cell in given direction
        :else (recur (mapv + pos dir) dir (update visited pos (fnil conj #{}) dir)))
      (update visited pos (fnil conj #{}) dir))))

(defn start-pos [grid]
  (reduce (fn [y row]
            (let [x (reduce (fn [x ch]
                              (if (= ch \^)
                                (reduced x)
                                (inc x))) 0 row)]
              (if (= x (count row)) (inc y) (reduced [y x]))))
          0
          grid))

(defn puzzle1 [input]
  (let [grid (mapv #(mapv identity %) input)
         pos (start-pos grid)]
    (count (keys (move grid pos)))))

(defn puzzle2 [input]
  (let [grid (mapv #(mapv identity %) input)
        pos (start-pos grid)
        route (keys (dissoc (move grid pos) pos))]
    (count (remove #(move (assoc-in grid % \#) pos) route))))
