(ns advent.2023.day8
  "Advent of Code 2023, day 8: Haunted Wasteland"
  (:require [advent.helpers :as h]
            [clojure.string :as str]))

(def puzzle-input (h/slurp-resource "2023/day8.txt" h/slurp-lines))

(defn mk-graph [nodes]
  (reduce (fn [acc [a l r]] (assoc acc a [l r])) {}
          (map (fn [node] (remove str/blank? (str/split node  #"[^\w]"))) nodes)))

(defn get-node [graph c d] (get-in graph [c (if (= d \L) 0 1)]))

;;; Part 1

(defn run-graph [graph ds]
  (loop [ds (cycle ds) i 0 c "AAA"]
    (if (= "ZZZ" c)
      i
      (recur (rest ds) (inc i) (get-node graph c (first ds))))))

(defn puzzle1 [[ds _ & nodes]]
  (run-graph (mk-graph nodes) ds))

;;; Part 2
;;; Finding whenever each individual path halts & returning
;;; lowest common multiplier of all halt indexes.

(defn gcd [x y]
  (if (zero? y)
    x
    (gcd y (mod x y))))

(defn lcm [x y]
  (/ (* x y) (gcd x y)))

(defn run-graph2 [graph dirs]
  (loop [dirs (cycle dirs)
         i 0
         found-lcm 1
         c (filter #(str/ends-with? % "A") (keys graph))]
    (if (seq c)
      (recur (rest dirs)
             (inc i)
             (if (some #(str/ends-with? % "Z") c) (lcm found-lcm i) found-lcm)
             (->> (remove #(str/ends-with? % "Z") c)
                  (map #(get-node graph % (first dirs)))))
      found-lcm)))

(defn puzzle2 [[dirs _ & nodes]]
  (run-graph2 (mk-graph nodes) dirs))
