(ns advent.2015.day4
  "Advent of Code 2015, day 4: The Ideal Stocking Stuffer"
  (:require [clojure.string :as str]
            [advent.helpers :as h])
  (:import (java.security MessageDigest)))

(def puzzle-input (h/slurp-resource "2015/day4.txt" h/slurp-line))

(def algo (delay (MessageDigest/getInstance "MD5")))

(defn md5 [^String s]
  (format "%032x" (BigInteger. 1 (.digest @algo (.getBytes s)))))

(defn find-hash-with-prefix [skey prefix]
  (reduce (fn [_ i]
            (when (str/starts-with? (md5 (str skey i)) prefix) (reduced i)))
          (range)))

(defn puzzle1 [input] (find-hash-with-prefix input "00000"))

(defn puzzle2 [input] (find-hash-with-prefix input "000000"))
