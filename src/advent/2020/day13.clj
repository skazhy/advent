(ns advent.2020.day13
  "Advent of Code 2020, day 13: Shuttle Search"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day13.txt" h/slurp-lines))

(defn closest-departure [interval t]
  (* interval (int (Math/ceil (/ t interval)))))

(defn find-closest-departure [t intervals]
  (->> (map (juxt #(closest-departure % t) identity) intervals) sort first))

(defn puzzle1 [input]
  (let [[t intervals] input
        [departure id] (find-closest-departure (Integer/parseInt t)
                                               (->> (str/split intervals #",")
                                                    (remove #(= "x" %))
                                                    (map #(Integer/parseInt %))))]
    (* id (- departure (Integer/parseInt t)))))

(defn parse2 [input]
  (loop [c 0 input (str/split input #",") out []]
    (if-let [i (first input)]
      (if (= "x" i)
        (recur (inc c) (rest input) out)
        (let [i (Integer/parseInt i)]
          (recur (inc c) (rest input) (conj out [i (- i c)]))))
      out)))

;; https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Example
;; https://brilliant.org/wiki/extended-euclidean-algorithm/
(defn extended-euclid
  [a b]
  (loop [s0 1 s1 0 r0 a r1 b]
    ;; Stop when we found a common denominator, or there are none.
    (if (and (< 1 r0) (not (zero? r1)))
      (let [q (quot r0 r1)]
        (recur s1 (- s0 (* q s1)) r1 (mod r0 r1)))
      s0)))

;; https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Existence_(constructive_proof)
;; https://brilliant.org/wiki/chinese-remainder-theorem/
(defn puzzle2 [[_ input]]
  (let [input (parse2 input)
        product (apply * (map first input))]
    (-> (reduce (fn [acc [n a]]
                  (let [y (quot product n)]
                    (+ acc (* a (extended-euclid y n) y))))
                0
                input)
        (mod product))))
