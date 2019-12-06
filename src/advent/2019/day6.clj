(ns advent.2019.day6
  "Advent of Code 2019, day 6: Universal Orbit Map"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2019/day6.txt" h/slurp-lines))

(defn orbital-graph [graph-step-fn input]
  (reduce graph-step-fn {} (map #(str/split % #"\)") input)))

;;; Puzzle 1: counting edges

(defn sum-of-edges [orbit-map k]
  (loop [k k c 1]
    (if-let [new-k (get orbit-map k)]
      (recur new-k (inc c))
      c)))

(defn directed-graph [input]
  (orbital-graph (fn [acc [inner outer]] (assoc acc outer inner)) input))

(defn puzzle1 [input]
  (let [orbit-map (directed-graph input)]
    (apply + (map (fn [[_ inner]] (sum-of-edges orbit-map inner)) orbit-map))))

;;; Puzzle 2: path length in a undirected graph

(defn undirected-graph [input]
  (orbital-graph  (fn [acc [inner outer]]
                    (-> (update acc inner #(conj (or % #{}) outer))
                        (update outer #(conj (or % #{}) inner))))
                  input))

(defn traverse [orb current destination visiting-from n]
  (let [conns (disj (get orb current) visiting-from)]
    (cond
      (conns destination) n
      (empty? conns) nil
      :else (->> (keep #(traverse orb % destination current (inc n)) conns)
                 (flatten)
                 (first)))))

(defn puzzle2 [input] (traverse (undirected-graph input) "YOU" "SAN" nil -1))
