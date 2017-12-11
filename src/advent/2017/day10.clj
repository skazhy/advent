(ns advent.2017.day10)

(defn day10 [inputs]
  (loop [lst (apply hash-map (mapcat (juxt identity identity) (range 256)))
         inputs inputs position 0 skip-size 0]
    (if-let [i (first inputs)]
      (let [rev-indexes (map #(mod (+ position %) 256) (range i))
            rev-items (->> (map #(get lst %) rev-indexes) reverse
                           (mapcat vector rev-indexes)
                           (apply hash-map))]
        ;(println rev-items)
        (recur (merge lst rev-items) (rest inputs) (+ position i skip-size) (inc skip-size)))
      lst)))

(def suffix [17 31 73 47 23])

(defn day10-2 [lst inputs position skip-size]
  (loop [lst lst inputs inputs position position skip-size skip-size]
    (if-let [i (first inputs)]
      (let [rev-indexes (map #(mod (+ position %) 256) (range i))
            rev-items (->> (map #(get lst %) rev-indexes) reverse
                           (mapcat vector rev-indexes)
                           (apply hash-map))]
        ;(println rev-items)
        (recur (merge lst rev-items) (rest inputs) (+ position i skip-size) (inc skip-size)))
      [lst position skip-size])))

(defn day10-outer [inputs]
  (loop [lst (apply hash-map (mapcat (juxt identity identity) (range 256)))
         position 0 skip-size 0 remaining 64]
    (if (pos? remaining)
      (let [[lst position skip-size] (day10-2 lst inputs position skip-size)]
        (recur lst position skip-size (dec remaining)))
      lst)))


(defn day10-multi [inputs]
  (->> (concat (mapv int inputs) suffix) day10-outer
       (sort-by first) (map second)
       (partition 16)
       (map #(format "%02x" (apply bit-xor %)))
       (apply str)))
