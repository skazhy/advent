(ns advent.2017.day16
  (:require [advent.helpers :as h]))

(def puzzle-input
  (h/slurp-resource "2017/day16.txt" (comp h/split-csv h/slurp-line)))

(defn gen [r] (mapv char (range 97 (+ 97 r))))

;;; Parsing

(def re-x #"x(\d+)/(\d+)")
(def re-p #"p(\w)/(\w)")
(def re-s #"s(\d+)")

(defn parse-exchange [s]
  (let [[_ a b] (re-matches re-x s)]
    [(Integer/parseInt a) (Integer/parseInt b)]))

(defn parse-spin [s]
  (let [[_ a] (re-matches re-s s)] [(Integer/parseInt a)]))

(defn parse-partner [s]
  (let [[_ a b] (re-matches re-p s)] [(first a) (first b)]))

(defn parse-instruction [i]
  (cond
    (.startsWith i "x") [:x (parse-exchange i)]
    (.startsWith i "p") [:p (parse-partner i)]
    :else [:s (parse-spin i)]))

;;; Performing moves

(defn exchange [l [a b]] (assoc l a (nth l b) b (nth l a)))
(defn partner [l [a b]] (assoc l (.indexOf l a) b (.indexOf l b) a))
(defn spin [l [i]] (into (vec (drop (- 16 i) l)) (take (- 16  i) l)))

(defn dance [r instructions]
  (reduce
   (fn [acc [op args]]
     (case op
       :x (exchange acc args)
       :p (partner acc args)
       :s (spin acc args)))
   r
   instructions))

;;; Puzzle 1

(defn puzzle1 [input]
  (->> (map parse-instruction input)
       (dance (gen 16))
       (apply str)))

;;; Puzzle 2

(defn unique-iterations [instructions]
  (loop [res (gen 16) seen #{} ins []]
    (if (contains? seen res)
      ins
      (recur (dance res instructions) (conj seen res) (conj ins res)))))

(defn puzzle2 [input]
  (let [instructions (map parse-instruction input)
        uniques (unique-iterations instructions)]
    (->> (count uniques)
         (mod 10000000)
         (nth uniques)
         (apply str))))
