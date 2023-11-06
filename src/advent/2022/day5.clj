(ns advent.2022.day5
  "Advent of Code 2022, day 5: Supply Stacks"
  (:require [advent.helpers :as h]
            [clojure.string :as str]))

(def puzzle-input (h/slurp-resource "2022/day5.txt" h/slurp-lines))

(defn parse-stack-row [row]
  (->> (partition-all 4 row)
       (map second)))

(defn parse-cmd-row [row]
  (let [s (str/split row #" ")]
    (map #(h/parse-int (nth s %)) [1 3 5])))

(defn run-cmds [input reorder]
  (let [stacks (->> (take-while (complement empty?) input)
                    butlast
                    (map parse-stack-row)
                    (apply map list)
                    (mapv (fn [s] (drop-while #(= \space %) s))))]
    (->> (drop-while (complement #(str/starts-with? % "move")) input)
         (map parse-cmd-row)
         (reduce (fn [stacks [n from to]]
                   (let [from (dec from)
                         to (dec to)]
                     (-> (update stacks to #(into % (reorder (take n (get stacks from)))))
                         (update from #(drop n %)))))
                 stacks)
         (map first)
         (apply str))))

(defn puzzle1 [input] (run-cmds input identity))
(defn puzzle2 [input] (run-cmds input reverse))
