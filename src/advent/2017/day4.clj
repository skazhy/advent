(ns advent.2017.day4
  "Advent of Code 2017, day 4: Unique passphrases"
  (:require [clojure.string :refer [join]]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2017/day4.txt" h/slurp-word-lines))

(defn- valid-passphrase? [mapper passphrase]
  (= (count passphrase) (count (distinct (map mapper passphrase)))))

(defn- valid-passphrases [mapper passphrases]
  (count (filter #(valid-passphrase? mapper %) passphrases)))

(defn puzzle1
  "Count lines with unique words"
  [passphrases]
  (valid-passphrases identity passphrases))

(defn puzzle2
  "Count lines wit no anagrams"
  [passphrases]
  (valid-passphrases #(join "" (map str (sort %))) passphrases))
