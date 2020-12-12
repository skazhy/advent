(ns advent.2020.day12
  "Advent of Code 2020, day 12: Rain Risk"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day12.txt" h/slurp-lines))

(defn parse-line [l]
  [(first l) (Integer/parseInt (apply str (drop 1 l)))])

(defn turn-right [[x y]] [(h/neg y) x])
(defn turn-around [[x y]] [(h/neg x) (h/neg y)])
(defn turn-left [[x y]] [y (h/neg x)])

(def right-turns [turn-right turn-around turn-left])

(defn actions [df]
  {\N (fn [course arg] (update-in course [df 1] - arg))
   \S (fn [course arg] (update-in course [df 1] + arg))
   \E (fn [course arg] (update-in course [df 0] + arg))
   \W (fn [course arg] (update-in course [df 0] - arg))
   \R (fn [course arg] (update course :dir (nth right-turns (dec (/ arg 90)))))
   \L (fn [course arg] (update course :dir (nth (reverse right-turns) (dec (/ arg 90)))))
   \F (fn [{:keys [dir] :as course} arg]
        (update course :coord #(apply mapv + % (take arg (repeat dir)))))})

(defn run-course [input dir waypoint-field]
  (let [actions (actions waypoint-field)]
    (loop [c {:coord [0 0] :dir dir} input (map parse-line input)]
      (if-let [[cmd arg] (first input)]
        (recur ((get actions cmd) c arg) (rest input))
        (apply + (map #(Math/abs %) (:coord c)))))))

(defn puzzle1 [input] (run-course input [1 0] :coord))
(defn puzzle2 [input] (run-course input [10 -1] :dir))
