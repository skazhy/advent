(ns advent.2023.day19
  "Advent of Code 2023, day 19: Aplenty
   Tags: incomplete"
  (:require [advent.helpers :as h]
            [clojure.string :as str]))

(def puzzle-input (h/slurp-resource "2023/day19.txt" h/slurp-lines))

(def workflow-re #"([\w]+)\{(.+)\}")
(def predicate-re #"([\w]+)(<|>)(.+)")
(def part-re #"\{(.+)\}")

(defn parse-workflow-part [part-str]
  (if-let [[_ field op val] (re-matches predicate-re part-str)]
    (fn [m]
      ((if (= "<" op) < >) (get m field) (h/parse-int val)))
    part-str))

(defn parse-workflows [workflows]
  (reduce (fn [acc workflow-str]
            (let [[_ name rules] (re-matches workflow-re workflow-str)]
                 (assoc acc name (map parse-workflow-part (str/split rules #",|:")))))
          {}
          workflows))

(defn final? [id]
  (or (= "A" id) (= "R" id)))

(defn run [workflows part-map]
  (loop [[pred then-wf & else] (get workflows "in")]
    (if (pred part-map)
      (if (final? then-wf)
        (= "A" then-wf)
        (recur (get workflows then-wf)))
      (if (< 1 (count else))
        (recur else)
        (if (final? (first else))
          (= "A" (first else))
          (recur (get workflows (first else))))))))

(defn parse-part [part-str]
  (let [parts (-> (re-matches part-re part-str)
                  last
                  (str/split #",|="))]
    (->> (partition 2 parts)
         (mapcat (fn [[k v]] [k (h/parse-int v)]))
         (apply hash-map))))

(defn puzzle1 [input]
  (let [[workflow-strs [_ & parts]] (split-with #(not= "" %) input)
        workflows (parse-workflows workflow-strs)]
    (->> (map parse-part parts)
         (filter #(run workflows %))
         (mapcat vals)
         (apply +))))
