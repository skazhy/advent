(ns advent.2017.day13)


(def ^:private row-pattern #"(\d+): (\d+)")

(defn parse-row [row]
  (->> (re-matches row-pattern row) rest (mapv #(Integer. %))))


(defn full-range [r]
  (let [ra (range r)]
    (into (vec ra) (reverse (butlast (rest ra))))))

(defn gen-lens [len-map int-matrix]
  (mapv #(get len-map %) (range (inc (first (last int-matrix))))))


;;; Puzzle 1

(defn gen-state [lens sec]
  (mapv #(when % (get % (mod sec (count %)))) lens))

(defn gen-states [lens]
  (mapv #(gen-state lens %) (range (count lens))))

(defn collisions
  [states]
  (filter #(= 0 (get-in states [% %])) (range (count states))))

(defn puzzle1 [rows]
  (let [int-matrix (mapv parse-row rows)
        lens (gen-lens int-matrix)]
    (->> (gen-states lens)
         collisions
         (map #(* % (inc (apply max (get lens %)))))
         (apply +))))


;;; Puzzle 2

(defn state [lens idx offset]
  (when-let [l (get lens idx)]
    (get l (mod (+ offset idx) (count l)))))

(defn has-collisions? [lens offset rng]
  (some #(= 0 (state lens % offset)) rng))

(defn puzzle2 [rows]
  (let [int-matrix (mapv parse-row rows)
        lens (gen-lens int-matrix)
        rng (range (count lens))]
    (loop [offset 0]
      (if (has-collisions? lens offset rng)
        (recur (inc offset))
        offset))))
