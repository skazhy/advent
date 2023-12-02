(ns advent.2023.day2
  "Advent of Code 2023, day 2: Cube Conundrum"
  (:require [advent.helpers :as h]
            [clojure.string :as str]))

(def puzzle-input (h/slurp-resource "2023/day2.txt" h/slurp-lines))

(defn parse-round [str-round]
  (->> (str/split str-round #"(,)? ")
       (partition 2)
       (reduce (fn [acc [count color]] (assoc acc (keyword color) (h/parse-int count))) {})))

(defn parse-game
  "Transforms string input line into a list of {:color count} maps."
  [str-game]
  (->> (str/split str-game #"[:;] ")
       (rest)
       (map parse-round)))

;;; Part 1 - Counting the games with rounds that contain less than max-vals items.

(def max-vals {:red 12 :green 13 :blue 14})

(defn valid-round? [round]
  (every? (fn [[color count]] (<= count (color max-vals))) round))

(defn valid-game? [rounds]
  (every? valid-round? rounds))

(defn puzzle1 [input]
  (apply + (map-indexed (fn [idx l] (if (valid-game? (parse-game l)) (inc idx) 0)) input)))

;;; Part 2 - Finding the largest amount of cubes needed per game, then multiplying the values.

(defn puzzle2 [input]
  (apply + (map #(->> % parse-game (apply merge-with max) vals (apply *)) input)))
