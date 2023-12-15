(ns advent.2023.day12
  "Advent of Code 2023, day 12: Hot Springs
   Tags: incomplete, slow"
  (:require [advent.helpers :as h]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.string :as str]))

(def puzzle-input (h/slurp-resource "2023/day12.txt" h/slurp-lines))

(def example ["???.### 1,1,3"
              ".??..??...?##. 1,1,3"
              "?#?#?#?#?#?#?#? 1,3,1,6"
              "????.#...#... 4,1,1"
              "????.######..#####. 1,6,5"
              "?###???????? 3,2,1"])

(defn replace [input pos mask]
  (reduce (fn [acc [idx repl]] (assoc acc idx repl)) input (map vector pos mask)))

(defn valid-replacement? [input lens]
  (->> (partition-by #(= \. %) input)
       (remove #(= \. (first %)))
       (map count)
       (= lens)))

(defn variation-count [input]
  (let [[input lens] (str/split input #" ")
        lens (map h/parse-int (str/split lens #","))
        input (vec input)
        pos (map first (filter #(= \? (last %)) (map-indexed vector input)))]
    (->> (apply cartesian-product (repeat (count pos) [\. \#]))
         (map #(replace input pos %))
         (filter #(valid-replacement? % lens))
         count)))

(defn puzzle1 [input]
  (apply + (map variation-count input)))
