(ns advent.2020.day14
  "Advent of Code 2020, day 14: Docking Data"
  (:require [clojure.math.combinatorics :refer [subsets]]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day14.txt" h/slurp-lines))

(def row-re #"(mask|mem\[(\d+)\]) = ([X\d]+)")

(defn new-block [mask] {:mask mask :ops []})

(defn grouped-lines [[f & tail]]
  (loop [input tail out [] curr (->> f (re-matches row-re) last new-block)]
    (if-let [i (first input)]
      (let [[_ k a v] (re-matches row-re i)]
        (if (= "mask" k)
          (recur (rest input) (conj out curr) (new-block v))
          (recur (rest input) out
                 (update curr :ops conj [(Integer/parseInt a) (Integer/parseInt v)]))))
      (conj out curr))))

(defn execute-blocks [input reducer]
  (->> (grouped-lines input)
       (reduce reducer {})
       (vals)
       (apply +)))

;;; Puzzle 1: value masks

(defn mask-fn [mask]
  (loop [idx 0 mask-fn identity mask (reverse mask)]
    (case (first mask)
      \1 (recur (inc idx) (comp #(bit-set % idx) mask-fn) (rest mask))
      \0 (recur (inc idx) (comp #(bit-clear % idx) mask-fn) (rest mask))
      \X (recur (inc idx) mask-fn (rest mask))
      nil mask-fn)))

(defn execute-block [acc {:keys [mask ops]}]
  (let [f (mask-fn mask)]
    (reduce (fn [a [k v]] (assoc a k (f v))) acc ops)))

(defn puzzle1 [input] (execute-blocks input execute-block))

;;; Puzzle 2: address masks

(defn mk-mask [mask bits]
  (reduce #(bit-set %1 %2) mask bits))

(defn masks [start mask]
  (loop [idx 0 res start mask (reverse mask) floating []]
    (case (first mask)
      \1 (recur (inc idx) (bit-set res idx) (rest mask) floating)
      \0 (recur (inc idx) res (rest mask) floating)
      \X (recur (inc idx) (bit-clear res idx) (rest mask) (conj floating idx))
      nil (conj (map #(mk-mask res %) (filter seq (subsets floating))) res))))

(defn execute-block2 [acc {:keys [mask ops]}]
  (reduce (fn [acc [k v]]
            (reduce (fn [acc k] (assoc acc k v)) acc (masks k mask)))
          acc
          ops))

(defn puzzle2 [input] (execute-blocks input execute-block2))
