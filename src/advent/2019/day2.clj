(ns advent.2019.day2
  "Advent of Code 2019, day 2: 1202 Program Alarm"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2019/day2.txt" h/slurp-int-csv-line))

(defn valid-op? [input offset]
  (let [op (nth input offset)]
    (when-not (= 99 op) op)))

(defn run-program [input noun verb]
  (loop [input (assoc (vec input) 1 noun 2 verb) offset 0]
    (if-let [op (valid-op? input offset)]
      (let [[a b res] (subvec input (inc offset) (+ 4 offset))
            op (case op 1 + 2 *)]
        (recur (assoc input res (op (nth input a) (nth input b)))
               (+ offset 4)))
      (first input))))

(defn puzzle1 [input] (run-program input 12 2))

(def ^:private puzzle2-output 19690720)

(defn puzzle2 [input]
  (let [base (run-program input 0 0)
        ; program is a linear function. "verb" argument delta is always 1.
        noun-delta (- (run-program input 1 0) base)
        noun (int (/ (- puzzle2-output base) noun-delta))]
    (+ (* 100 noun)
       (- puzzle2-output base (* noun noun-delta)))))
