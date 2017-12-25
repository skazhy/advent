(ns advent.2017.day19
  (:require [advent.helpers :as h]))


(def puzzle-input (h/slurp-resource "2017/day19.txt" h/slurp-lines))

(def pipes #{\| \-})

(defn possible-turns [[x y]]
  [[y x] [(h/neg y) (h/neg x)]])

(defn next-direction [maze direction c]
  (let [d (->> (possible-turns direction)
               (map (juxt identity #(get-in maze (mapv + c %))))
               (filter (comp identity second))
               (remove #(= \space (second %))) ffirst)]

    d))

(defn traverse-maze [rows]
  (let [maze (mapv #(mapv char %) rows)
        input [0 (dec (count (first rows)))]]
    (loop [c input direction [1 0] seen [] steps 1]
      (let [new-coord (mapv + c direction)
            v (get-in maze new-coord nil)]
        (if (and v (not= \space v))
          (cond
            (contains? pipes v) (recur new-coord direction seen (inc steps))
            (= \+ v) (recur new-coord (next-direction maze direction new-coord)
                                      seen
                                      (inc steps))
            :else (recur new-coord direction (conj seen v) (inc steps)))
          [seen steps])))))

(defn puzzle1 [rows]
  (->> (traverse-maze rows) first (apply str)))

(defn puzzle2 [rows]
  (second (traverse-maze rows)))
