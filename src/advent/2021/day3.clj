(ns advent.2021.day3
  "Advent of Code 2021, day 3: Day 3: Binary Diagnostic"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2021/day3.txt" h/slurp-lines))

(defn bin->dec [s]
  (read-string (apply str "2r" s)))

(defn frequencies-at-idx
  "Returns a sorted list of frequencies: [[\1 10] [\0 3]]"
  [report idx]
  (sort-by val > (frequencies (map #(nth % idx) report))))

(defn high-low [report idx]
  (map first (frequencies-at-idx report idx)))

(defn puzzle1 [input]
  (let [res (map #(high-low input %) (range (count (first input))))]
    (* (bin->dec (map first res)) (bin->dec (map last res)))))

(defn rating [report tiebreaker-bit get-hl-fn]
  (loop [idx 0 report report]
    (let [[hi lo :as hi-lo] (frequencies-at-idx report idx)
          bit (if (= (last hi) (last lo))
                tiebreaker-bit
                (first (get-hl-fn hi-lo)))
          res (filter #(= bit (nth % idx)) report)]
      (if (< 1 (count res))
        (recur (inc idx) res)
        (bin->dec (first res))))))

(defn puzzle2 [input]
  (* (rating input \1 first) (rating input \0 last)))
