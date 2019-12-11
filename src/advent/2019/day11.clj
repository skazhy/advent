(ns advent.2019.day11
  "Advent of Code 2019, day 11: A Puzzle"
  (:require [clojure.core.async :refer [go-loop <!! <!] :as async]
            [advent.helpers.intcode :refer [make-input-channel
                                            run-program-async]]
            [advent.helpers :as h]))

 (def puzzle-input
   (h/slurp-resource "2019/day11.txt" (comp vec h/slurp-int-csv-line)))

(defn update-direction [i [x y]]
  (if (zero? i)
    [(* -1 y) x]  ; Left turn: [0 1] -> [-1 0] -> [0 -1] -> [1 0] -> [0 1]
    [y (* -1 x)]))  ; Right turn: [0 1] -> [1 0] -> [0 -1] -> [-1 0] -> [0 1]

(defn run-painter-robot [program initial-color callback]
  (let [input-chan (make-input-channel [])
        output-chan (async/chan)]
    (run-program-async program input-chan output-chan)
    (go-loop [coordinate [0 0]
              direction [0 1]
              painted #{}
              grid {coordinate initial-color}]
      (async/put! input-chan (get grid coordinate 0))
      (if-let [color (<! output-chan)]
        (let [direction (-> output-chan <! (update-direction direction))]
          (recur (mapv + coordinate direction)
                 direction
                 (conj painted coordinate)
                 (assoc grid coordinate color)))
        (callback painted grid)))))

(defn puzzle1 [program]
  (<!! (run-painter-robot program 0 (fn [p _] (count p)))))

;;;

(defn min-max-range [coll]
  (let [mn (apply min coll)
        mx (apply max coll)]
    (range mn (inc mx))))

(defn puzzle2 [program]
  (let [grid (<!! (run-painter-robot program 1 (fn [_ g] g)))
        x-range (min-max-range (map first (keys grid)))]
    (->> (min-max-range (map last (keys grid)))
         (map (fn [y]
                (apply str (map #(if (= 1 (get grid [% y])) "#" " ") x-range))))
         (reverse))))
