(ns advent.puzzles
  (:require [advent.helpers :refer [digit-seq reduce-indexed]]))


;;; Day 1

(defn inverse-captcha
  "Day 1, puzzle 1: Inverse CAPTCHA"
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

(defn halfway-captcha
  "Day 1, puzzle 2: Halfway CAPTCHA"
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
