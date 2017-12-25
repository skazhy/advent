(ns advent.2017.day25
  "Advent of Code 2017, day 25: Turing Machine")


; TODO: rewrite this to actually parse the file.
(def puzzle-input
  {:a {0 {:val 1 :op inc :to-state :b} 1 {:val 0 :op dec :to-state :c}}
   :b {0 {:val 1 :op dec :to-state :a} 1 {:val 1 :op dec :to-state :d}}
   :c {0 {:val 1 :op inc :to-state :d} 1 {:val 0 :op inc :to-state :c}}
   :d {0 {:val 0 :op dec :to-state :b} 1 {:val 0 :op inc :to-state :e}}
   :e {0 {:val 1 :op inc :to-state :c} 1 {:val 1 :op dec :to-state :f}}
   :f {0 {:val 1 :op dec :to-state :e} 1 {:val 1 :op inc :to-state :a}}})

(defn apply-rules [machine rules]
  (let [pos (:position machine)
        rule (get (get rules (:state machine))
                  (get-in machine [:tape pos] 0))]
    (-> (assoc-in machine [:tape pos] (:val rule))
        (update :position (:op rule))
        (assoc :state (:to-state rule)))))

(defn puzzle1 [rules steps]
  (loop [machine {:tape {} :position 0 :state :a} steps steps]
    (if (pos? steps)
      (recur (apply-rules machine rules) (dec steps))
      (reduce-kv (fn [acc _ v] (+ acc v)) 0 (:tape machine)))))
