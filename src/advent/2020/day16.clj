(ns advent.2020.day16
  "Advent of Code 2020, day 16: Ticket Translation"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day16.txt" h/slurp-lines))

(def ranges-re #"([a-z\s]+): (\d+)-(\d+) or (\d+)-(\d+)")

(defn field-checker [range-row]
  (let [[_ field & ranges] (re-matches ranges-re range-row)
        [a b x y] (map #(Integer/parseInt %) ranges)]
    {field #(or (<= a % b) (<= x % y))}))

(defn parse-num-range [row] (map #(Integer/parseInt %) (str/split row #",")))

(defn parse-input [input]
  (let [[ranges _ ticket _ nearby] (partition-by #(not= % "") input)]
    {:field-checkers (reduce merge {} (map field-checker ranges))
     :ticket (parse-num-range (last ticket))
     :nearby (->> nearby rest (map parse-num-range))}))

(defn valid-field-set [checkers n]
  (set (map first (filter (comp #(% n) val) checkers))))

(defn field-choices [checkers ticket]
  (map #(valid-field-set checkers %) ticket))

(defn field-indexes [field-choices]
  (loop [unpicked field-choices picked {}]
    (if (seq unpicked)
      (let [[idx field] (first (filter #(= 1 (count (val %))) unpicked))
            field (first field)]
        (recur (reduce-kv (fn [acc k v] (assoc acc k (disj v field)))
                          {}
                          (dissoc unpicked idx))
               (assoc picked field idx)))
      picked)))

(defn puzzle1 [input]
  (let [{:keys [field-checkers nearby]} (parse-input input)
        checker-fn (apply some-fn (vals field-checkers))]
    (reduce #(apply + %1 (remove checker-fn %2)) 0 nearby)))

(defn puzzle2 [input]
  (let [{:keys [field-checkers ticket nearby]} (parse-input input)
        checker-fn (apply some-fn (vals field-checkers))
        ;; Remove all invalid rows, then create a map from number index to
        ;; set of all possible field choices for this field.
        field-choices (->> (remove #(seq (remove checker-fn %)) nearby)
                           (map #(field-choices field-checkers %))
                           (apply map set/intersection)
                           (map-indexed vector)
                           (into {}))
        indexes (->> (field-indexes field-choices)
                     (filter #(.startsWith (key %) "departure"))
                     (map val))]
    (apply * (map #(nth ticket %) indexes))))
