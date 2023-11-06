(ns advent.2016.day4
  "Advent of Code 2016, day 4: Security Through Obscurity"
  (:require [advent.helpers :as h]
            [clojure.string :as str]))

(def puzzle-input (h/slurp-resource "2016/day4.txt" h/slurp-lines))

(defn checksum [room]
  (->> (dissoc (frequencies room) \-)
       (sort-by (juxt val (comp #(* -1 %) int key)))
       (reverse)
       (map key)
       (take 5)
       (apply str)))

(defn decode
  "Apply a Caesar cipher to the characters ofa given room string."
  [room id]
  [(->> (map #(if (= \- %) %
                (char (+ (mod (+ (- (int %) 97) id) 26) 97)))
             room)
        (apply str))
   id])

(defn valid-room? [row]
  (let [[_ room id sum] (re-matches #"([a-z-]+)(\d+)\[([a-z0-9]+)\]" row)]
    (when (= sum (checksum room))
      [room (h/parse-int id)])))

(defn puzzle1 [input]
  (reduce (fn [acc [_ id]] (+ acc id)) 0 (keep valid-room? input)))

(defn puzzle2 [input]
  (->> (keep valid-room? input)
       (map #(apply decode %))
       (filter #(str/starts-with? (first %) "north"))
       first
       last))
