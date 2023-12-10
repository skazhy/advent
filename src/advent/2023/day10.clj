(ns advent.2023.day10
  "Advent of Code 2023, day 10: Pipe Maze
   Tags: incomplete"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2023/day10.txt" h/slurp-lines))

(def con-map
  {[0 -1] #{\- \L \F \S}
   [0 1] #{\- \J \7 \S}
   [-1 0] #{\| \F \7 \S}
   [1 0] #{\| \J \L \S}})

(def neighbor-coords
  "Neighbor [y x] deltas for the given element."
  {\| [[-1 0] [1 0]]
   \- [[0 -1] [0 1]]
   \L [[-1 0] [0 1]]
   \J [[0 -1] [-1 0]]
   \7 [[1 0] [0 -1]]
   \F [[0 1] [1 0]]
   \. []
   \S [[0 -1] [-1 0] [0 1] [1 0]]})

(defn neighbors [grid coords]
  (->> (get-in grid coords)
       (get neighbor-coords)
       (keep (fn [d]
               (let [c (map + coords d)
                     n (get-in grid c)]
                 (when (contains? (get con-map d) n)
                   c))))))

(defn find-start-coords [grid]
  (loop [y 0]
    (if-let [x (->> (nth grid y)
                    (map-indexed vector)
                    (filter #(= \S (last %)))
                    ffirst)]
      [y x]
      (recur (inc y)))))

(defn pipe-coords [input]
  (let [start (find-start-coords input)
        [up down] (neighbors input start)]
    (loop [up up
           down down
           visited #{up down start}]
      (let [new-up (first (remove visited (neighbors input up)))
            new-down (first (remove visited (neighbors input down)))]
        (if (and new-up new-down)
          (recur new-up new-down (conj visited new-up new-down))
          visited)))))

(defn puzzle1 [input] (/ (count (pipe-coords input)) 2))
