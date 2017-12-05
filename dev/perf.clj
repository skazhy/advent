(ns perf
  "Puzzle sollution performance testing."
  (:require [clojure.java.io :refer [resource]]
            [advent.puzzles :as p]
            [advent.helpers :as h]))


(defmacro measure [[m :as form]]
  `(do
    (println "Measuring" (:name (meta (var ~m))))
    (dorun (repeatedly 5 (fn [] (time ~form))))))


(defn day-1-perf []
  (let [input (h/slurp-line (resource "day1.txt"))]
    (measure (p/inverse-captcha input))
    (measure (p/halfway-captcha input))))

(defn day-2-perf []
  (let [input (h/slurp-int-matrix (resource "day2.txt"))]
    (measure (p/corruption-checksum input))
    (measure (p/evenly-divisible-checksum input))))

(defn day-3-perf []
  (measure (p/manhattan-distance 279138))
  (measure (p/taxicab-neighbor-sum  279138)))

(defn day-4-perf []
  (let [input (h/slurp-word-lines (resource "day4.txt"))]
    (measure (p/unique-passphrases input))
    (measure (p/no-anagram-passphrases input))))

(defn day-5-perf []
  (let [input (vec (h/slurp-int-lines (resource "day5.txt")))]
    (measure (p/inc-jumps input))
    (measure (p/inc-dec-jumps input))))
