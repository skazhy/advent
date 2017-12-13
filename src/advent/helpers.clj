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

(defn slurp-line [path]
  (trim (slurp path)))

(defn slurp-lines [path]
  (-> (slurp path) (split #"\n")))

;; Reading strings

(defn slurp-word-lines [path]
  (map #(split (trim %) #" ") (slurp-lines path)))

;; Reading numbers

(defn slurp-int-lines [path]
  (map #(Integer. %) (slurp-lines path)))

(defn slurp-int-line [path]
  (map #(Integer. %) (first (slurp-lines path))))

(defn slurp-int-matrix [path]
  (map (fn [line] (map  #(Integer. %) (split line #"\s|\t")))
       (slurp-lines path)))

(defn split-csv [s]
  (split s #","))
