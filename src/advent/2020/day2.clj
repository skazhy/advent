(ns advent.2020.day2
  "Advent of Code 2020, day 2: Password Philosophy"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day2.txt" h/slurp-lines))

(defn parse-input [row]
  (let [[low high letter pattern] (str/split row #":*\s|-")]
    {:low (Integer/parseInt low)
     :high (Integer/parseInt high)
     :letter (first letter)
     :password pattern}))

(defn valid-password? [{:keys [password letter low high]}]
  (<= low (count (filter #(= letter %) password)) high))

(defn puzzle1 [input]
  (->> (map parse-input input) (filter valid-password?) count))

(defn valid-password-2? [{:keys [password letter low high]}]
  (not= (= letter (nth password (dec low)))
        (= letter (nth password (dec high)))))

(defn puzzle2 [input]
  (->> (map parse-input input) (filter valid-password-2?) count))
