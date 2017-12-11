(ns perf
  "Puzzle sollution performance testing."
  (:require [clojure.java.io :refer [resource]]
            [advent.helpers :as h]
            [advent.2017.day1 :as day1]
            [advent.2017.day2 :as day2]
            [advent.2017.day3 :as day3]
            [advent.2017.day4 :as day4]
            [advent.2017.day5 :as day5]
            [advent.2017.day6 :as day6]
            [advent.2017.day7 :as day7]
            [advent.2017.day8 :as day8]
            [advent.2017.day9 :as day9]))


(defmacro measure [[m :as form]]
  `(do
    (println "Measuring" (:name (meta (var ~m))))
    (dorun (repeatedly 5 (fn [] (time ~form))))))


(defn day-1-perf []
  (let [input (h/slurp-line (resource "2017/day1.txt"))]
    (measure (day1/puzzle1 input))
    (measure (day1/puzzle2 input))))

(defn day-2-perf []
  (let [input (h/slurp-int-matrix (resource "2017/day2.txt"))]
    (measure (day2/puzzle1 input))
    (measure (day2/puzzle2 input))))

(defn day-3-perf []
  (measure (day3/puzzle1 279138))
  (measure (day3/puzzle2  279138)))

(defn day-4-perf []
  (let [input (h/slurp-word-lines (resource "2017/day4.txt"))]
    (measure (day4/puzzle1 input))
    (measure (day4/puzzle2 input))))

(defn day-5-perf []
  (let [input (vec (h/slurp-int-lines (resource "2017/day5.txt")))]
    (measure (day5/puzzle1 input))
    (measure (day5/puzzle1 input))))

(defn day-6-perf []
  (let [input (vec (first (h/slurp-int-matrix (resource "2017/day6.txt"))))]
    (measure (day6/puzzle1 input))
    (measure (day6/puzzle1 input))))

(defn day-7-perf []
  (let [input (h/slurp-word-lines (resource "2017/day7.txt"))]
    (measure (day7/puzzle1 input))
    (measure (day7/puzzle2 input))))

(defn day-8-perf []
  (let [input (h/slurp-word-lines (resource "2017/day8.txt"))]
    (measure (day8/puzzle1 input))
    (measure (day8/puzzle2 input))))

(defn day-9-perf []
  (let [input (h/slurp-line (resource "2017/day9.txt"))]
    (measure (day9/puzzle1 input))
    (measure (day9/puzzle2 input))))


(defn -main [day-no & _]
  (case day-no
    "1" (day-1-perf)
    "2" (day-2-perf)
    "3" (day-3-perf)
    "4" (day-4-perf)
    "5" (day-5-perf)
    "6" (day-6-perf)
    "7" (day-7-perf)
    "8" (day-8-perf)
    "9" (day-9-perf)
    nil))
