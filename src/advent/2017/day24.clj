(ns advent.2017.day24
  (:require [advent.helpers :as h]))


(defn parse-line [r]
  (let [[a b] (clojure.string/split r #"\/")]
    [(Integer. a) (Integer. b)]))

(def puzzle-input
  (h/slurp-resource "2017/day24.txt" h/slurp-lines))

(defn port-map [ports]
  (reduce
    (fn [acc [f s]]
      (cond-> acc
        (not (zero? s)) (update f #(conj (or % #{}) s))
        (not (zero? f)) (update s #(conj (or % #{}) f))))
    {}
    ports))

(defn strength [bridge]
  (apply + (into bridge (butlast bridge))))

(defn build-bridges [ports bridge bridge-mapper bridge-comparator]
  (let [l (last bridge)]
    (if-let [vs (not-empty (get ports l))]
      (->> vs
           (map
             (fn [k]
               (build-bridges (-> (update ports k #(disj % l))
                                   (update l #(disj % k)))
                              (conj bridge k)
                              bridge-mapper
                              bridge-comparator)))
           (bridge-comparator))
        (bridge-mapper bridge))))


;;; Puzzle 1

(defn puzzle1 [ports]
  (build-bridges (port-map (map parse-line ports)) [0] strength #(apply max %)))


;;; Puzzle 2

(defn strongest-longest [bridges]
  (reduce (fn [acc i]
            (cond
              (< (count acc) (count i)) i
              (< (count i) (count acc)) acc
              (< (strength acc) (strength i)) i
              :else acc))
            bridges))

(defn puzzle2 [ports]
  (-> (map parse-line ports)
      (port-map)
      (build-bridges [0] identity strongest-longest)
      (strength)))
