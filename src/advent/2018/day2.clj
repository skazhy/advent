(ns advent.2018.day2
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2018/day2.txt" h/slurp-lines))

(defn item-counts [acc r]
  (let [counts (->> (reduce (fn [acc c] (update acc c (fnil inc 0))) {} r)
                    (vals)
                    (set))]
    (cond-> acc
      (contains? counts 2) (update :two inc)
      (contains? counts 3) (update :three inc))))

(defn puzzle1 [input]
  (loop [counts {:two 0 :three 0} input input]
    (if (seq input)
      (recur (item-counts counts (first input)) (rest input))
      (* (:two counts) (:three counts)))))
;;;

(defn common-str
  "Given strings a and b, returns string of characters common in both,
   if there is only one differing character."
  [a b]
  (let [common (filter identity (map #(when (= %1 %2) %1) a b))]
    (when (= (count a) (inc (count common)))
      (apply str common))))

;;; Probably can be done nicer or with list comprehension, to avoid
;;; comparing string to itself.

(defn puzzle2 [input]
  (some (fn [a] (some #(common-str a %) input)) input))
