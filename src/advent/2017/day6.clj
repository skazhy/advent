(ns advent.2017.day6
  "Advent of Code 2017, day 6: Sequence redistribution")


(defn- max-idx-item [items]
  (->> (map-indexed vector items)
       (reduce (fn [b item] (if (< (second b) (second item)) item b)))))

(defn redistribute-all [items max-idx max-item pl]
  (let [r (rem max-item (dec (count items)))]
    (map-indexed (fn [i item] (if (= i max-idx) r (+ item pl))) items)))

(defn redistribute-subset [items max-item max-idx]
  (->> (range (inc max-idx) (+ max-item (inc max-idx)))
       (map #(mod % (count items)))
       (reduce
         (fn [items idx] (update items idx inc))
         (assoc (vec items) max-idx 0))))

(defn redistribute [items]
  (let [[max-idx max-item] (max-idx-item items)
        pl (quot max-item (dec (count items)))]
    (if (pos? pl)
      (redistribute-all items max-idx max-item pl)
      (redistribute-subset items max-item max-idx))))

(defn puzzle1
  [items]
  (loop [items items seen #{} c 1]
    (let [res (redistribute items)]
      (if (contains? seen res)
        c
        (recur res (conj seen res) (inc c))))))

(defn puzzle2
  [items]
  (loop [items items seen #{} c 1]
    (let [res (redistribute items)]
      (if (contains? seen res)
        (dec (puzzle1 res))
        (recur res (conj seen res) (inc c))))))
