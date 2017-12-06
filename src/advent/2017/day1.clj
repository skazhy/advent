(ns advent.2017.day1
  "Advent of Code 2017, day 1: CAPTCHAs"
  (:require [advent.helpers :refer [digit-seq reduce-indexed]]))


;;; Finding the sum of identical adjacent numbers.

(defn puzzle1
  "Inverse CAPTCHA"
  [captcha]
  (let [digits (digit-seq captcha)]
    (first
      (reduce
        (fn [[sum prev] digit]
          (if (= digit prev)
            [(+ sum digit) digit]
            [sum digit]))
        [0 (last digits)]
        digits))))


;;; Finding equal numbers halfway ahead in a circular list.

(defn puzzle2
  "Halfway CAPTCHA"
  [captcha]
  (let [digits (digit-seq captcha)
        digit-count (count digits)
        hw (int (Math/ceil (/ digit-count 2)))]
    (reduce-indexed
      (fn [idx sum d]
        ; If given number is equal to the number half way around the circular
        ; list - add it to the sum.
        (if (= d (nth digits (mod (+ hw idx) digit-count)))
          (+ sum d)
          sum))
      0
      digits)))
