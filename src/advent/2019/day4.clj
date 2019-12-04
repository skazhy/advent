(ns advent.2019.day4
  "Advent of Code 2019, day 4: Secure Container"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2019/day4.txt" h/slurp-line))

(defn parse-input [input] (map #(Integer/parseInt %) (str/split input #"-")))

(defn str-range [rng] (map str (apply range rng)))

(defn increasing-digits? [digit-str]
  (apply <= (map int digit-str)))

(defn grouped-some [pred digit-str]
  (some pred (partition-by identity digit-str)))

(defn has-a-group? [digit-str] (grouped-some #(< 1 (count %)) digit-str))

(defn has-group-of-two? [digit-str] (grouped-some #(= (count %) 2) digit-str))

(defn puzzle1 [input]
  (->> (parse-input input)
       (str-range)
       (filter (every-pred increasing-digits? has-a-group?))
       (count)))

(defn puzzle2 [input]
  (->> (parse-input input)
       (str-range)
       (filter (every-pred increasing-digits? has-group-of-two?))
       (count)))
