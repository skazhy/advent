(ns advent.2017.day15
  "Advent of Code 2017, day 15: Number generators")


(def ^:private a-multiplier 16807)
(def ^:private b-multiplier 48271)

(defn generate [mul i] (rem (* mul i) 2147483647))

(defn- comp-bin [[a b]]
  (= (bit-and a 65535) (bit-and b 65535)))

(defn- gen-pairs [start-a start-b gen-a gen-b]
  (iterate (fn [[a b]] [(gen-a a) (gen-b b)]) [start-a start-b]))

(defn- count-matching-values [start-a start-b gen-a gen-b size]
  (->> (gen-pairs start-a start-b gen-a gen-b)
       (take size)
       (reduce (fn [acc vs] (if (comp-bin vs) (inc acc) acc)) 0)))


;; Puzzle 1

(defn puzzle1 [start-a start-b]
  (count-matching-values start-a start-b
                         (partial generate a-multiplier)
                         (partial generate b-multiplier)
                         40000000))


;; Puzzle 2

(defn- generate-2 [mul db i]
  (let [res (generate mul i)]
    (if (zero? (rem res db)) res (generate-2 mul db res))))

(defn puzzle2 [start-a start-b]
  (count-matching-values start-a start-b
                         (partial generate-2 a-multiplier 4)
                         (partial generate-2 b-multiplier 8)
                         5000000))
