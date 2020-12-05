(ns advent.2020.day5
  "Advent of Code 2020, day 5: Binary Boarding"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day5.txt" h/slurp-lines))

(defn binary-partition [low-char high-limit char-seq]
  (first
   (reduce (fn [[low high :as rng] l]
             (let [delta (/ (- high low) 2)]
               (if (= low-char l)
                 (update rng 1 - delta)
                 (update rng 0 + delta))))
           [0 high-limit]
           char-seq)))

(def decode-row (partial binary-partition \F 128))
(def decode-seat (partial binary-partition \L 8))

(defn decode-boarding-pass [bp]
  (let [[row seat] (map #(%1 %2) [decode-row decode-seat] (partition-all 7 bp))]
    (+ (* 8 row) seat)))

(defn puzzle1 [input] (apply max (map decode-boarding-pass input)))

(defn puzzle2 [input]
  (->> (map decode-boarding-pass input)
       sort
       (reduce (fn [acc i]
                 (if (= (inc acc) i) i (reduced (inc acc)))))))
