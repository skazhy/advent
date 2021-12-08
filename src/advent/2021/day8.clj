(ns advent.2021.day8
  "Advent of Code 2021, day 8: Seven Segment Search"
  (:require [clojure.set :as set]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2021/day8.txt" h/slurp-word-lines))

(def unique-lengths #{2 3 4 7})

(def pattern-count->digit {2 1, 3 7, 4 4, 7 8})

(defn decode-record [input]
  (let [[patterns output] (-> (split-at 10 (map set input))
                              (update 1 rest))
        known (reduce (fn [acc p]
                        (if-let [d (pattern-count->digit (count p))]
                          (assoc acc d (set p))
                          acc))
                      {}
                      patterns)
        zero-six-nine (filter #(= 6 (count %)) patterns)
        six (first (filter #(= 1 (count (set/difference (get known 1) %))) zero-six-nine))
        nine (first (filter #(zero? (count (set/difference (get known 4) % ))) zero-six-nine))
        known (assoc known
                     0 (first (filter #(and (not= six %) (not= nine %)) zero-six-nine))
                     3 (first (filter #(and (= 5 (count %)) (empty? (set/difference (get known 1) %))) patterns))
                     6 six
                     9 nine)
        [five two] (sort-by #(count (set/difference (get known 9) %))
                            (filter #(and (= 5 (count %)) (not= % (get known 3))) patterns))
        known (set/map-invert (assoc known 5 five 2 two))]
    (h/parse-int (apply str (map known output)))))

(defn puzzle1 [input]
  (->> (mapcat #(drop 11 %) input)
       (filter #(contains? unique-lengths (count %)))
       (count)))

(defn puzzle2 [input] (apply +  (map decode-record input)))
