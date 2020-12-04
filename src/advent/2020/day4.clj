(ns advent.2020.day4
  "Advent of Code 2020, day 4: Passport Processing"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day4.txt" h/slurp-lines))

(defn parse-line [str-line]
  (apply hash-map (str/split str-line #":|\s")))

(defn parse-lines [input]
  (loop [passports [] passport {} input input]
    (if-let [i (first input)]
      (if (str/blank? i)
        (recur (conj passports passport) {} (rest input))
        (recur passports (merge passport (parse-line i)) (rest input)))
      (conj passports passport))))

;;; Validation

(def eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn validate-int-range-field [v low high]
  (-> (Integer/parseInt v) (#(<= low % high))))

(defn validate-height [v]
  (when-let [[_ height m] (re-matches #"(\d+)(cm|in)" v)]
    (if (= "cm" m)
      (validate-int-range-field height 150 193)
      (validate-int-range-field height 59 76))))

(def field-validators
  {"byr" #(validate-int-range-field % 1920 2002)
   "iyr" #(validate-int-range-field % 2010 2020)
   "eyr" #(validate-int-range-field % 2020 2030)
   "hgt" validate-height
   "hcl" #(re-matches #"#([0-9a-f]{6})" %)
   "pid" #(re-matches #"[0-9]{9}" %)
   "ecl" #(contains? eye-colors %)})

;;; Puzzles

(defn puzzle1 [input]
  (let [passports (parse-lines input)
        required-fields (set (keys field-validators))]
    (count (filter #(every? % required-fields) passports))))

(defn validate-passport [passport]
  (every? (fn [[field validator]] (some-> (get passport field) validator))
          field-validators))

(defn puzzle2 [input]
  (->> (parse-lines input) (filter validate-passport) count))
