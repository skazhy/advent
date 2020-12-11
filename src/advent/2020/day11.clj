(ns advent.2020.day11
  "Advent of Code 2020, day 11: Seating System"
  (:require [advent.helpers :as h]
            [advent.helpers.grid :refer [all-neighbors]]))

(def puzzle-input (h/slurp-resource "2020/day11.txt" h/slurp-lines))

(def seat? #{\# \L})

(defn parse-input [input]
  (reduce
   (fn [acc line-idx]
     (reduce
      (fn [acc col-idx]
        (->> (nth (nth input line-idx) col-idx)
             (seat?)
             (assoc acc [line-idx col-idx])))
      acc
      (range (count (first input)))))
   {}
   (range (count input))))

(defn update-seat [state idx seat-counter tolerance]
  (case (get state idx)
    nil nil
    \# (if (< tolerance (seat-counter state idx)) \L \#)
    \L (if (zero? (seat-counter state idx)) \# \L)))

(defn update-state [state seat-counter tolerance]
  (reduce
   (fn [acc idx]
     (assoc acc idx (update-seat state idx seat-counter tolerance)))
   {}
   (keys state)))

(defn stable-state-occupied-seats [input seat-counter tolerance]
  (loop [state (parse-input input) i 1]
    (let [newstate (update-state state seat-counter tolerance)]
      (if (= state newstate)
        (count (filter #(= \# (val %)) newstate))
        (recur newstate (inc i))))))

(defn taken-seat-count [seats]
  (count (filter #(= % \#) seats)))

;;; Puzzle 1: Adjacent neighbors

(defn adjacent-taken-seat-count [state seat-idx]
  (->> all-neighbors
       (map #(get state (mapv + seat-idx %)))
       (taken-seat-count)))

(defn puzzle1 [input]
  (stable-state-occupied-seats input adjacent-taken-seat-count 3))

;;; Puzzle 2: Visible neighbors

(defn first-visible-neighbor [state seat-idx v]
  (loop [seat (mapv + seat-idx v)]
    (when (contains? state seat)
      (or (get state seat)
          (recur (mapv + seat v))))))

(defn visible-taken-seat-count [state seat-idx]
  (->> all-neighbors
       (map #(first-visible-neighbor state seat-idx %))
       (taken-seat-count)))

(defn puzzle2 [input]
  (stable-state-occupied-seats input visible-taken-seat-count 4))
