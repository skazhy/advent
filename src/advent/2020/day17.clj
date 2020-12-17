(ns advent.2020.day17
  "Advent of Code 2020, day 17: Conway Cubes"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day17.txt" h/slurp-lines))

(defn neighbor-state [grid neighbors coords]
  (->> (map #(get grid (mapv + coords %)) neighbors)
       (filter identity)
       (count)))

(defn update-cell [grid neighbors acc coords]
  (let [s (neighbor-state grid neighbors coords)]
    (if (get grid coords)
      (assoc acc coords (or (= 2 s) (= 3 s)))
      (assoc acc coords (= 3 s)))))

(defn update-range [[f l]] [(dec f) (inc l)])

(defn run-game-of-life [update-w-range neighbors lines]
  (loop [x-range [0 (count (first lines))]
         y-range [0 (count lines)]
         z-range [0 1]
         w-range [0 1]
         c 0
         grid (into {}
                    (for [x (apply range x-range) y (apply range y-range)]
                      [[x y 0 0] (= \# (get-in lines [y x]))]))]
    (let [x-range (update-range x-range)
          y-range (update-range y-range)
          z-range (update-range z-range)
          w-range (update-w-range w-range)]
      (if (< c 6)
        (recur x-range y-range z-range w-range (inc c)
               (reduce #(update-cell grid neighbors %1 %2)
                       {}
                       (for [x (apply range x-range)
                             y (apply range y-range)
                             z (apply range z-range)
                             w (apply range w-range)]
                         [x y z w])))
        (count (filter val grid))))))

(defn neighbors [w-range]
  (for [x [-1 0 1] y [-1 0 1] z [-1 0 1] w w-range
        :when (not= 0 x y z w)] [x y z w]))

(defn puzzle1 [input]
  (run-game-of-life (constantly [0 1]) (neighbors [0]) input))

(defn puzzle2 [input]
  (run-game-of-life update-range (neighbors [-1 0 1]) input))
