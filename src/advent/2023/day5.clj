(ns advent.2023.day5
  "Advent of Code 2023, day 5: If You Give A Seed A Fertilizer"
  (:require [clojure.string :as str]))

(defn slurp-blocks [str-input]
  (->> (str/split (slurp str-input) #"\n\n") (map #(str/split % #"\n"))))

(defn parse-map [row]
  (let [[dest src len] (map parse-long (str/split row #" "))]
    [dest src (+ src (dec len))]))

(defn parse-blocks [rows]
  (map parse-map (rest rows)))

(defn map-int [[dest src-low src-high] r]
  (when (<= src-low r src-high)
    (+ dest (- r src-low))))

;;; Part 1 - mapping integers

(defn int-source->dest-fn
  "Creates a mapper function for the given iput block."
  [ranges]
  (fn [x]
    (or (some #(map-int % x) ranges) x)))

(defn puzzle1 [[[seed-row] & blocks]]
  (let [seeds (map parse-long (rest (str/split seed-row #" ")))
        seed->location (apply comp (reverse (map (comp int-source->dest-fn parse-blocks) blocks)))]
    (apply min (map seed->location seeds))))

(defn parse-seed-row [row]
  (->> (map parse-long (rest (str/split row #" ")))
       (partition 2)
       (map (fn [[a b]] [a (dec (+ a b))]))))

;;; Part 2 - mapping inclusive ranges.

(defn map-range
  "Given a [i-low i-high] input range and a [dest m-low m-high] mapper range,
   returns a pair of mapped intersection of i and m + vector of unmodified differences of i and m."
  [[dest m-low m-high] [i-low i-high]]
  (cond
    ;; Out of bounds
    (or (< i-high m-low) (< m-high i-low)) [[[i-low i-high]] nil]
    ;; Subset
    (and (<= m-low i-low) (<= i-high m-high))
    [[] [(+ dest (- i-low m-low))  (+ dest (- i-low m-low) (- i-high i-low))]]
    ;; i end overlaps m
    (and (< i-low m-low) (<= i-high m-high))
    [[[i-low (dec m-low)]]
     [dest (+ dest (- i-high m-low))]]
    ;; i start overlaps
    (and (<= m-low i-low m-high) (< m-high i-high))
    [[[(inc m-high) i-high]]
     [(+ dest (- i-low m-low)) (+ dest (- m-high m-low))]]
    ;; Superset
    :else [[[i-low (dec m-low)]
            [(inc m-high) i-high]]
           [dest (+ dest (- m-high m-low))]]))

(defn range-source->dest-fn
  [mappers]
  (fn [unprocessed]
    (loop [mappers mappers unprocessed unprocessed found []]
      (if-let [m (first mappers)]
        (let [results (map #(map-range m %) unprocessed)]
          (if-let [unprocessed (seq (mapcat first results))]
            (recur (rest mappers)
                   unprocessed
                   (into found (keep last results)))
            (into found (keep last results))))
        (into found unprocessed)))))

(defn puzzle2 [[[seed-row] & blocks]]
  (let [seeds (parse-seed-row seed-row)
        seeds->location (->> (map (comp range-source->dest-fn parse-blocks) blocks)
                             reverse
                             (apply comp))]
    (->> (mapcat #(seeds->location [%]) seeds)
         (map first)
         (apply min))))
