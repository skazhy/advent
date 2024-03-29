(ns advent.2017.day21
  "Advent of Code 2017, day 21: Fractal Art
   Tags: slow"
  (:require [advent.helpers :as h]
            [clojure.string :as str]))

(def puzzle-input (h/slurp-resource "2017/day21.txt" h/slurp-lines))

(defn flip-horizontal [grid n] (vec (mapcat reverse (partition n grid))))

(defn flip-vertical [grid n]
  (into (vec (drop (* n (dec n)) grid)) (take (* n (dec n)) grid)))

(defn rotate [grid n]
  (if (= 3 n)
    (map #(nth grid %) [6 3 0 7 4 1 8 5 2])
    (map #(nth grid %) [2 0 3 1])))

(def init [[\. \# \.] [\. \. \#] [\# \# \#]])

(defn parse-rule [row]
  (let [[a _ b] (str/split row #"\s")]
    [a b]))

(defn parse-rules [rows]
  (apply hash-map (mapcat parse-rule rows)))

(defn pairs [n]
  (let [rng (range n)] (mapcat (fn [r] (map #(vector r %) rng)) rng)))

(defn expand-one [n [x y]]
  (->> (range (* x n) (* n (inc x)))
       (mapcat (fn [x] (map #(vector x %) (range (* y n) (* (inc y) n)))))))

(defn expand [gridlen l]
  (map #(expand-one l %) (pairs (/ gridlen l))))

(defn squash [grid subsec l]
  (let [res
        (->> (map #(get-in grid %) subsec)
             (partition l)
             (map #(apply str %)) (clojure.string/join "/"))]
    res))

(defn replace-rot [grid subsec l patterns]
  (loop [rotations 0 s subsec]
    (when (< rotations 5)
      (or (get patterns (squash grid s l))
          (recur (inc rotations) (rotate s l))))))

(defn replace-fliprot [grid subsec l patterns]
  (loop [ho? false ve? false s subsec]
    (or (replace-rot grid s l patterns)
        (case [ho? ve?]
          [false false] (recur true false (flip-horizontal subsec l))
          [true false] (recur false true (flip-vertical subsec l))
          [false true] (replace-rot grid
                                    (flip-horizontal (flip-vertical subsec l) l)
                                    l
                                    patterns)))))

(defn div-by [grid]
  (if (even? (count (first grid))) 2 3))

(def lols {3 4 2 3})

(defn unpack-square [s]
  (->> (str/split s #"\/")
       (mapv #(mapv char %))))

(defn join-squares [acc sq n]
  (if (< (count (last acc)) n)
    (reduce
     (fn [acc [rel idx]] (update acc idx #(into % (nth sq rel))))
     acc
     (map-indexed vector (range (- (count acc) (count sq)) (count acc))))
    (into acc sq)))

(defn taken-squares [rules iters]
  (let [rules (parse-rules rules)]
    (loop [g init iters iters]
      (if (pos? iters)
        (let [gridlen (count (first g))
              n (div-by g)
              subsquares (expand gridlen n)
              replaced (map #(replace-fliprot g % n rules) subsquares)
              r (* (/ gridlen n) (get lols (div-by g)))
              newg (->> (mapv unpack-square replaced)
                        (reduce (fn [acc x] (join-squares acc x r))))]
          (recur newg (dec iters)))
        (->> (flatten g)
             (reduce (fn [acc x] (if (= \# x) (inc acc) acc)) 0))))))

(defn puzzle1 [rows] (taken-squares rows 5))
(defn puzzle2 [rows] (taken-squares rows 18))
