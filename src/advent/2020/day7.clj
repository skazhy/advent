(ns advent.2020.day7
  "Advent of Code 2020, day 7: A Puzzle"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day7.txt" h/slurp-lines))

(def re-toplevel #"^([a-z]+ [a-z]+) bags contain ([\w\s,]+).$")
(def re-dependency #"(\d+) ([a-z]+ [a-z]+) bags?")

(defn parse-rows
  [rows dependency-reduce-step]
  (reduce
   (fn [acc row]
     (let [[_ color deps] (re-matches re-toplevel row)]
       (if (= "no other bags" deps)
         acc
         (reduce (fn [acc d]
                   (let [[_ n c] (re-matches re-dependency d)]
                     (dependency-reduce-step acc color c (Integer/parseInt n))))
                 acc
                 (str/split deps #", ")))))
   {}
   rows))

;;; Puzzle 1: reverse search

(defn reduce-step-1 [acc toplevel-color color n]
  (update acc color assoc toplevel-color n))

(defn parents-for-color [m color]
  (if-let [parent-colors (keys (get m color))]
    (conj (mapcat #(parents-for-color m %) parent-colors) color)
    [color]))

(defn puzzle1 [input]
  (->> (parents-for-color (parse-rows input reduce-step-1) "shiny gold")
       (distinct)
       (count)
       (dec)))

;;; Puzzle 2: Sum of values

(defn reduce-step-2 [acc toplevel-color color n]
  (assoc-in acc [toplevel-color color] n))

(defn sum-of-contents [m color]
  (if-let [child-colors (get m color)]
    (apply + 1 (map #(* (val %) (sum-of-contents m (key %))) child-colors))
  1))

(defn puzzle2 [input]
  (->> (sum-of-contents (parse-rows input reduce-step-2) "shiny gold")
       (dec)))
