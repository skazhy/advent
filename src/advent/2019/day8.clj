(ns advent.2019.day8
  "Advent of Code 2019, day 8: Space Image Format"
  (:require [advent.helpers :as h]))

(def w 25)
(def h 6)

(def puzzle-input (h/slurp-resource "2019/day8.txt" h/slurp-line))

(defn image-layers [input w h]
  (partition-all (* w h) input))

(defn puzzle1 [input]
  (let [freqs (->> (image-layers input w h)
                   (sort-by (comp count (partial filter #(= \0 %))))
                   (first)
                   (frequencies))]
    (*  (get freqs \1) (get freqs \2))))

;;;

(defn transparent? [pixel] (= \2 pixel))

(defn compose-pixels [pixels] (or (first (remove transparent? pixels)) \2))

(defn compose-layers [layers]
  (apply map (fn [& pixels] (compose-pixels pixels)) layers))

(defn format-layer [layer w]
  (->> (partition w (map #(if (= % \0) " " "X") layer))
       (map #(apply str %))))

(defn puzzle2 [input]
  (format-layer (compose-layers (image-layers input w h)) w))
