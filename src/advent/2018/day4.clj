(ns advent.2018.day4
  "Advent of Code 2018, day 4: Repose Record"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2018/day4.txt" h/slurp-lines))

(def row-pattern #"\[\d{4}-(\d{2})-(\d{2}) (\d{2}):(\d{2})] (.+)")

(def guard-no-pattern #"Guard #(\d+) begins shift")

; XXX: No February dates or December 31 in the input file.
(def month-ends
  {1 31 2 28 3 31 4 30 5 31 6 30 7 31 8 31 9 30 10 31 11 30 12 31})

(defn parse-row [raw-row]
  (let [[_ mo d h mi action] (re-matches row-pattern raw-row)
        d (Integer. d)
        mo (Integer. mo)]
    (cond
      (= "00" h) [[mo d] (Integer. mi) action]
      (= (month-ends mo) d) [[(inc mo) 1] 0 action]
      :else [[mo (inc d)] 0 action])))

(defn asleep-ranges [sleep-actions]
  (->> (map second sleep-actions) (partition 2) (map #(apply range %))))

(defn guard-id [action]
  (-> (re-matches guard-no-pattern (last action)) (last) (Integer.)))

(defn sleep-map [input]
  (->> (partition-by first (map parse-row (sort input)))
       (reduce (fn [sleep-map logs]
                 (if-let [actions (seq (rest logs))]
                   (update sleep-map
                           (guard-id (first logs))
                           into (asleep-ranges actions))
                   sleep-map))
               {})))

;;;

(defn most-asleep
  "Finds the id and the sleep ranges for the guard, who has slept the most."
  [sleep-map]
  (first (sort-by #(apply + (map count (val %))) > sleep-map)))

(defn puzzle1 [input]
  (let [[guard-id sleep-ranges] (most-asleep (sleep-map input))
        minute-asleep (->> (flatten sleep-ranges)
                           (reduce (fn [acc i] (update acc i (fnil inc 0))) {})
                           (sort-by val >) ffirst)]
    (* guard-id minute-asleep)))

;;;

(defn sleep-frequencies [[id sleep-ranges]]
  [id (->> (flatten sleep-ranges)
           (reduce (fn [acc i] (update acc i (fnil inc 0))) {})
           (sort-by last >) first)])

(defn puzzle2 [input]
  (->> (sleep-map input)
       (map sleep-frequencies)
       (sort-by #(-> % second last) >) first
       (#(* (first %) (-> % second first)))))
