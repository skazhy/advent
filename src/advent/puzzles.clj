(ns advent.puzzles
  (:require [clojure.string :refer [join split]]
            [advent.helpers :as h]))


;;; Day 1: CAPTCHAs

;; Finding the sum of identical adjacent numbers.

(defn inverse-captcha
  "Day 1, puzzle 1: Inverse CAPTCHA"
  [captcha]
  (let [digits (h/digit-seq captcha)]
    (first
      (reduce
        (fn [[sum prev] digit]
          (if (= digit prev)
            [(+ sum digit) digit]
            [sum digit]))
        [0 (last digits)]
        digits))))

;; Finding equal numbers halfway ahead in a circular list.

(defn halfway-captcha
  "Day 1, puzzle 2: Halfway CAPTCHA"
  [captcha]
  (let [digits (h/digit-seq captcha)
        digit-count (count digits)
        hw (int (Math/ceil (/ digit-count 2)))]
    (h/reduce-indexed
      (fn [idx sum d]
        ; If given number is equal to the number half way around the circular
        ; list - add it to the sum.
        (if (= d (nth digits (mod (+ hw idx) digit-count)))
          (+ sum d)
          sum))
      0
      digits)))


;;; Day 2: Checksums

;; Finding difference of min and max values in a row

(defn- row-extreme-sum [row]
  (- (apply max row) (apply min row)))

(defn corruption-checksum
  "Day 2, puzzle 1: Corruption checksum"
  [int-matrix]
  (reduce (fn [acc row] (+ acc (row-extreme-sum row))) 0 int-matrix))

;; Finding the sum of 2 evenly divisible numbers in a row

(defn- evenly-divisible? [a b]
  (when (and (pos? a) (pos? b))
    (if (> a b)
      (when (zero? (mod a b)) (/ a b))
      (when (zero? (mod b a)) (/ b a)))))

(defn- evenly-divisible-row [row]
  (-> (reduce
        (fn [past item]
          (if-let [res (first (keep #(evenly-divisible? % item) past))]
            (reduced {:result res})
            (conj past item)))
        [] row)
      (:result 0)))

(defn evenly-divisible-checksum
  "Day 2, puzzle 2: Evenly divisible checksum"
  [int-matrix]
  (reduce (fn [acc row] (+ acc (evenly-divisible-row row))) 0 int-matrix))


;;; Day 3: Taxicab geometry

;; "Turning" = flipping the grid
;;
;; pos(ition) [x y] in the grid
;; dir(ection) [x y] - the current movement
;; direction x: -1 left / 0 no horizontal movement / 1 right
;; direction y: -1 down / 0 no vertical movement / 1 up
;;
;; Move from 1 -> 2 is treated like a turn around bottom left corner, so
;; we always start with downward direction.

(defn- turn-up? [[x y]] (and (pos? x) (= x (- 1 y))))
(defn- turn-down? [[x y]] (and (neg? x) (= x (h/neg y))))
(defn- turn-left? [[x y]] (= x y))

(def origin-pos [0 0])

;; Bruteforce Manhattan distance calculation between (0,0) and a given point.

(defn- update-direction
  "Updates direction based on the current position."
  [pos dir]
  (if (or (turn-up? pos) (turn-down? pos) (turn-left? pos))
    [(h/neg (second dir)) (first dir)] ; Do a 90o turn by flipping directions
    dir))

(defn manhattan-distance
  "Day 3, puzzle 1: Manhattan distance"
  [n]
  (let [[pos] (reduce
                (fn [[pos dir] _]
                  (let [dir (update-direction pos dir)]
                    [(mapv + pos dir) dir]))
                [origin-pos [0 -1]]
                (range (dec n)))]
    (apply + (map #(Math/abs %) pos))))

;; Calculating a taxicab point value

(def ^:private neighbors
  [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn- neighbor-sum
  "Returns the sum of all neighboring cell values."
  [grid [x y]]
  (apply + (map (fn [[nx ny]] (get grid [(+ x nx) (+ y ny)] 0)) neighbors)))

(defn taxicab-neighbor-sum
  "Day 3, puzzle 1: Neighbor sums"
  [n]
  (loop [pos origin-pos dir [0 -1] grid {}]
    (let [cell-val (neighbor-sum grid pos)
          dir (update-direction pos dir)
          grid (if (= origin-pos pos)
                     ; Handle empty grids correctly.
                     (assoc grid pos 1)
                     (assoc grid pos cell-val))]
      (if (< n cell-val)
        cell-val
        (recur (mapv + pos dir) dir grid)))))


;;; Day 4: Unique passphrases

(defn- valid-passphrase? [mapper passphrase]
  (= (count passphrase) (count (distinct (map mapper passphrase)))))

(defn- valid-passphrases [mapper passphrases]
  (count (filter #(valid-passphrase? mapper %) passphrases)))

(defn unique-passphrases
  "Day 4, puzzle 1: count lines with unique words"
  [passphrases]
  (valid-passphrases identity passphrases))

(defn no-anagram-passphrases
  "Day 4, puzzle 2: count lines wit no anagrams"
  [passphrases]
  (valid-passphrases #(join "" (map str (sort %))) passphrases))


;;; Day 5: Sequence jumps

(defn- jump [mapper items pos]
  (let [v (+ pos (get items pos))]
    (when (get items v)
      [(update items pos mapper) v])))

(defn- jump-count
  "Given a sequence of jumps and a mapping function, count the max possible
   jumps, starting from the first instruction."
  [mapper items]
  (loop [items items pos 0 n 1]
    (if-let [[items pos] (jump mapper items pos)]
      (recur items pos (inc n))
      n)))

(defn inc-jumps
  "Day 5, puzzle 2: Jumps with increasing offsets"
  [items]
  (jump-count inc items))

(defn inc-dec-jumps
  "Day 5, puzzle 2: Jumps with decreasing large offsets"
  [items]
  (jump-count #(if (<= 3 %) (dec %) (inc %)) items))


;;; Day 6: Sequence redistribution

(defn- max-idx-item [items]
  (->> (map-indexed vector items)
       (reduce (fn [b item] (if (< (second b) (second item)) item b)))))

(defn redistribute-all [items max-item pl]
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
        pl (quot max-item (dec (count items)))
    (if (pos? pl)
      (redistribute-all items max-item pl)
      (redistribute-subset items max-idx-item max-idx))))

(defn redistribution-count
  [items]
  (loop [items items seen #{} c 1]
    (let [res (redistribute items)]
      (if (contains? seen res)
        c
        (recur res (conj seen res) (inc c))))))

(defn redistribution-loop-size
  [items]
  (loop [items items seen #{} c 1]
    (let [res (redistribute items)]
      (if (contains? seen res)
        (dec (redistribution-count res))
        (recur res (conj seen res) (inc c))))))
