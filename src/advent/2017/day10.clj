(ns advent.2017.day10
  "Advent of Code 2017, day 1: Knot hashes")


(defn knot-round
  [lst inputs position skip-size range-size]
  (loop [lst lst inputs inputs position position skip-size skip-size]
    (if-let [i (first inputs)]
      (let [rev-indexes (map #(mod (+ position %) range-size) (range i))
            rev-items (->> (map #(get lst %) rev-indexes) reverse
                           (mapcat vector rev-indexes)
                           (apply hash-map))]
        (recur (merge lst rev-items)
               (rest inputs)
               (+ position i skip-size)
               (inc skip-size)))
      [lst position skip-size])))

(defn puzzle1 [input-str l]
  (let [inputs (->> (clojure.string/split input-str #",") (map #(Integer. %)))
        lst (apply hash-map (mapcat (juxt identity identity) (range l)))
        res (first (knot-round lst inputs 0 0 l))]
    (* (get res 0) (get res 1))))


;;; Puzzle 2

(def ^:private salt [17 31 73 47 23])

(defn- knot-rounds [inputs]
  (loop [lst (apply hash-map (mapcat (juxt identity identity) (range 256)))
         position 0 skip-size 0 remaining 64]
    (if (pos? remaining)
      (let [[lst position skip-size]
            (knot-round lst inputs position skip-size 256)]
        (recur lst position skip-size (dec remaining)))
      lst)))

(defn knot-hash [input-str]
  (->> (concat (mapv int input-str) salt) knot-rounds
       (sort-by first) (map second)
       (partition 16)
       (map #(format "%02x" (apply bit-xor %)))
       (apply str)))

(defn puzzle2 [input-str] (knot-hash input-str))
