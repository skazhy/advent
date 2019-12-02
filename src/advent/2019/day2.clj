(ns advent.2019.day2
  "Advent of Code 2019, day 2: 1202 Program Alarm"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2019/day2.txt" h/slurp-line))

(defn run-program [input noun verb]
  (loop [input (assoc input 1 noun 2 verb) offset 0]
    (let [op (nth input offset)]
      (if (= 99 op)
        (first input)
        (let [op (case op 1 + 2 *)
              a (nth input (inc offset))
              b (nth input (+ 2 offset))
              res (nth input (+ 3 offset))]
          (recur (assoc input res (op (nth input a) (nth input b)))
                 (+ offset 4)))))))

(defn puzzle1 [input]
  (run-program (mapv #(Integer/parseInt %) (str/split input #",")) 12 2))

(def ^:private puzzle2-output 19690720)

(defn puzzle2 [input]
  (let [input (mapv #(Integer/parseInt %) (str/split input #","))
        ; program is a linear function.
        base (run-program input 0 0)
        noun-delta (- (run-program input 1 0) base)
        verb-delta (- (run-program input 0 1) base) ; is it always 1?
        noun (int (/ (- puzzle2-output base) noun-delta))]
    (+ (* 100  noun)
       (- puzzle2-output base (* noun noun-delta)))))
