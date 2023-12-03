(ns advent.2023.day3
  "Advent of Code 2023, day 3: Gear Ratios"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2023/day3.txt" h/slurp-lines))

(defn int-char? [x] (< 47 (int x) 59))

(defn parse-input
  "Transforms input row in the {:ops [] :nums []} map
  :ops vector contains all valid operation [x y] coordinates
  :maps vector contains vector for each row, containing [number x-start x-end] for each number on the line."
  [input valid-op?]
  (reduce (fn [acc [y str-row]]
            (loop [row (partition-by int-char? str-row)
                   i 0
                   acc (update acc :nums conj [])]
              (if-let [el (first row)]
                (recur
                 (rest row)
                 (+ i (count el))
                 (if (int-char? (first el))
                   (update-in acc [:nums y] conj [(h/parse-int (apply str el)) (dec i) (+ i (count el))])
                   (update acc :ops into (->> (map-indexed (fn [idx l]
                                                             (when (valid-op? l) [(+ i idx) y])) el)
                                              (filter identity)))))
                acc)))
          {:ops [] :nums []}
          (map-indexed vector input)))

(defn in-num-bounds? [x [_ min-x max-x]]
  (<= min-x x max-x))

;;; Part 1 - Counting the sum of all numbers that are connected by ops.

(defn connected-numbers [x row]
  (reduce (fn [acc [n :as num]] (if (in-num-bounds? x num) (+ acc n) acc))
          0
          row))

(defn puzzle1 [input]
  (let [{:keys [nums ops]} (parse-input input #(not= % \.))]
    (loop [sum 0
           ops ops]
      (if-let [[x y] (first ops)]
        (recur
         (reduce (fn [s row-idx]
                   (+ s (connected-numbers x (nth nums row-idx))))
                 sum
                 (range (max (dec y) 0) (min (count nums) (+ 2 y))))
         (rest ops))
        sum))))

;;; Part 2 - Finding numbers that are connected via '*' op & then multiplying them.

(defn product [res]
  (if (< 1 (count res))  ;; Skip * operations connected to only one number
    (apply * (map first res))
    0))

(defn puzzle2 [input]
  (let [{:keys [nums ops]} (parse-input input #(= % \*))]
    (loop [sum 0
           ops ops]
      (if-let [[x y] (first ops)]
        (recur (->> (range (max (dec y) 0) (min (count nums) (+ 2 y)))
                    (mapcat (fn [row-idx] (filter #(in-num-bounds? x %) (nth nums row-idx))))
                    product
                    (+ sum))
               (rest ops))
        sum))))
