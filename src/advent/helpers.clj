(ns advent.helpers
  (:require [clojure.string :refer [split trim]]))


(defn neg [x] (* -1 x))

(defn digit-seq
  "Takes number as a string and returns a sequence with all digits as integers."
  [int-str]
  (map #(Integer. (str %)) int-str))


(defn reduce-indexed
  [f acc items]
  (first
    (reduce
      (fn [[acc idx] item] [(f idx acc item) (inc idx)])
      [acc 0]
      items)))


;;; I/O

(defn slurp-lines [path]
  (-> (slurp path) (split #"\n")))

(defn slurp-int-lines [path]
  (map #(Integer. %) (slurp-lines path)))

(defn slurp-word-lines [path]
  (map #(split (trim %) #" ") (slurp-lines path)))

(defn slurp-int-matrix [path]
  (map (fn [line] (map  #(Integer. %) (split line #" ")))
       (slurp-lines path)))
