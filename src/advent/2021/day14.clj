(ns advent.2021.day14
  "Advent of Code 2021, day 14: Extended Polymerization"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2021/day14.txt" h/slurp-lines))

(defn parse-rule [rule]
  (let [[_ a b c] (re-matches #"(\w)(\w) -> (\w)" rule)]
    [(str a b) [(str a c)(str c b)]]))

(defn template->freqs [template]
  (frequencies (map #(apply str %) (partition 2 1 template))))

(defn grow-polymer [rules template]
  (reduce-kv (fn [acc k v]
               (reduce (fn [acc rule] (update acc rule (fnil + 0) v))
                       acc
                       (get rules k)))
             {}
             template))

(defn polymer-frequencies [[template _ & rules] n]
  (let [rules (into {} (map parse-rule rules))]
    (->> (nth (iterate #(grow-polymer rules %) (template->freqs template)) n)
         (reduce (fn [acc [[a] n]] (update acc a (fnil + 0) n)) {(last template) 1})
         ((juxt #(apply max (vals %)) #(apply min (vals %))))
         (apply -))))

(defn puzzle1 [input] (polymer-frequencies input 10))
(defn puzzle2 [input] (polymer-frequencies input 40))
