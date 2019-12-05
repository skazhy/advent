(ns advent.2019.day2
  "Advent of Code 2019, day 2: 1202 Program Alarm"
  (:require [advent.helpers :as h]
            [advent.helpers.intcode :refer [run-program]]))

(def puzzle-input
  (h/slurp-resource "2019/day2.txt" (comp vec h/slurp-int-csv-line)))

;;; All interesting bits for this puzzle are in the incode implementation.

(defn run* [input noun verb]
  (-> (assoc input 1 noun 2 verb)
      (run-program [])
      (:program) first))

(defn puzzle1 [input] (run* input 12 2))

(def ^:private puzzle2-output 19690720)

(defn puzzle2 [input]
  (let [base (run* input 0 0)
        ; program is a linear function. "verb" argument delta is always 1.
        noun-delta (- (run* input 1 0) base)
        noun (int (/ (- puzzle2-output base) noun-delta))]
    (+ (* 100 noun)
       (- puzzle2-output base (* noun noun-delta)))))
