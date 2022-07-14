(ns advent.helpers
  (:require [clojure.set :as set]
            [clojure.string :refer [split trim]]
            [clojure.java.io :refer [resource]]))

(defn neg [x] (* -1 x))

(defn digit-seq
  "Takes number as a string and returns a sequence with all digits as integers."
  [int-str]
  (map #(Integer/parseInt (str %)) int-str))

(defn reduce-indexed
  [f acc items]
  (first
   (reduce
    (fn [[acc idx] item] [(f idx acc item) (inc idx)])
    [acc 0]
    items)))

(defn split-lines [lines]
  (map #(clojure.string/split % #" ") lines))

(defn split-csv [s]
  (split s #","))

(defn intersection [coll-a coll-b] (set/intersection (set coll-a) (set coll-b)))

;;; I/O

(defn slurp-resource [path mapper]
  (mapper (resource path)))

(defn slurp-line [path]
  (trim (slurp path)))

(defn slurp-lines [path]
  (-> (slurp path) (split #"\n")))

;; Reading strings

(defn slurp-word-lines [path]
  (map #(split (trim %) #" ") (slurp-lines path)))

(defn slurp-csv-lines [path]
  (map split-csv (slurp-lines path)))

;; Reading numbers

(defn slurp-int-lines [path]
  (map #(Integer/parseInt %) (slurp-lines path)))

(defn slurp-int [path]
  (Integer/parseInt (first (slurp-lines path))))

(defn parse-int-matrix [lines]
  (map (fn [line]
         (map #(Integer/parseInt %) (split (trim line) #"[\s|\t]+")))
       lines))

(defn slurp-int-matrix [path] (parse-int-matrix (slurp-lines path)))

(defn parse-int [str-i]
  (try (Integer/parseInt str-i)
       (catch Exception _ (BigInteger. str-i))))

(defn slurp-int-csv-line [path]
  (->> (slurp-line path) (split-csv) (map parse-int)))
