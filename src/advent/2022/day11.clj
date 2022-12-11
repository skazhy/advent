(ns advent.2022.day11
  "Advent of Code 2022, day 11: Monkey in the Middle"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2022/day11.txt" h/slurp-lines))

(defn parse-op-row [row]
  (let [[a f b] (str/split (.substring row 19) #"\s")]
    (fn [old] ((if (= f "*") * +) (if (= "old" a) old (h/parse-int a)) (if (= "old" b) old (h/parse-int b))))))

(defn parse-entry [[_ start op check if-true if-false]]
  {:items (mapv h/parse-int (drop 4 (str/split start #":*,*\s")))
   :op (parse-op-row op)
   :check (h/parse-int (.substring check 21))
   :inspected 0
   :if-true (h/parse-int (.substring if-true 29))
   :if-false (h/parse-int (.substring if-false 30))})

(defn parse [rows]
  (map parse-entry (partition-all 7 rows)))

(defn run-turn [monkeys idx item]
  (let [item ((get-in monkeys [idx :op]) item)
        throw-to (if (zero? (rem item (get-in monkeys [idx :check])))
                   (get-in monkeys [idx :if-true]) (get-in monkeys [idx :if-false]))]
    (-> (update-in monkeys [idx :items] (comp vec rest))
        (update-in [idx :inspected] inc)
        (update-in [throw-to :items] conj item))))

(defn run-turns [monkeys idx]
  (reduce (fn [monkeys item] (run-turn monkeys idx item))
          (assoc-in monkeys [idx :items] [])
          (get-in monkeys [idx :items])))

(defn run-rounds [n monkeys]
  (loop [n n monkeys (zipmap (range) monkeys)]
    (if (pos? n)
      (recur (dec n) (reduce run-turns monkeys (sort (keys monkeys))))
      (->> (map (comp :inspected val) monkeys)
           (sort >)
           (take 2)
           (apply *)))))

(defn puzzle1 [input]
  (->> (parse input)
       (mapv (fn [m] (update m :op (fn [op] (comp #(int (Math/floor (/ % 3))) op)))))
       (run-rounds 20)))

(defn puzzle2 [input]
  (let [monkeys (parse input)
        check-product (apply * (map :check monkeys))]
    (->> monkeys
         (mapv (fn [m] (update m :op (fn [op] (comp #(rem % check-product) op)))))
         (run-rounds 10000))))
