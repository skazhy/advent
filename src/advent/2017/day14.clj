(ns advent.2017.day14
  (:require [advent.2017.day10 :as d10]
            [clojure.set :refer [difference]]))

;;;; Grid is represented as n * n vector of vectors.
;;;;
;;;; Initial values (0 1) are represented as strings - taken segments
;;;; as their segment numbers.

; TODO: can I use `format` in a sane fashion here?
(def hex->bin
  {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100" \5 "0101"
   \6 "0110" \7 "0111" \8 "1000" \9 "1001" \a "1010" \b "1011"
   \c "1100" \d "1101" \e "1110" \f "1111"})

(defn gen-grid [in-str]
  (mapv (comp #(mapv str %)
              #(mapcat hex->bin %)
              #(d10/puzzle2 (str in-str "-" %)))
        (range 128)))


;;; Puzzle 1 - Counting all taken coordinates

(defn puzzle1 [in-str]
  (->> (gen-grid in-str) flatten (filter #(= "1" %)) count))


;;; Puzzle 2 - counting all segments

(def ^:private neighbors [[-1 0] [0 -1] [0 1]  [1 0]])

(defn new-segment?  [grid c] (= "1" (get-in grid c)))

(defn pairs [n]
  (mapcat (fn [r] (map #(vector r %) (range n))) (range n)))

(defn neighbors-in-segment
  "Returns a set of neighbor coordinates that are in the same segment."
  [grid  c]
  (->> (map #(mapv + % c) neighbors)
       (filter #(new-segment? grid %))
       (set)))

(defn segment
  "Returns coordinates for a segment that contains given coordinates."
  [grid start-coord]
  (loop [c start-coord backlog #{} seen #{}]
    (if-let [new-neighbors (-> (neighbors-in-segment grid c)
                               (difference seen)
                               (not-empty))]
      (let [combi (into backlog new-neighbors)
            f (first combi)]
        (recur f (disj combi f) (conj seen c)))
          (if-let [f (first backlog)]
            (recur f (disj backlog f) (conj seen c))
            (conj seen c)))))

(defn segment-count [grid]
  (loop [grid grid
         coords (pairs (count (first grid)))
         counter 0]
    (if-let [c (first coords)]
      (if (new-segment? grid c)
        (let [cs (segment grid c)]
          ; XXX: segment counter is reused to mark already computed segements
          ;      this is redundant, but left here for testing.
          (recur (reduce (fn [grid c] (assoc-in grid c counter)) grid cs)
                 (rest coords)
                 (inc counter)))
        (recur grid (rest coords) counter))
      counter)))

(defn puzzle2 [in-str]
  (segment-count (gen-grid in-str)))
