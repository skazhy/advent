(ns advent.helpers.intcode
  "Intcode helpers (for 2019 puzzles)"
  (:require [clojure.core.async :refer [put! <! <!! go] :as async]))

(defn parse-opcode
  "Takes an intcode and returns it split to digits with the opcode
   in the first position."
  [{:keys [program offset]}]
  (let [raw-op (nth program offset)
        opcode (rem raw-op 100)]
    [opcode (map #(Integer/parseInt (str %)) (str (int (/ raw-op 100))))]))

(defn padded-modes [modes t immediate-last?]
  (let [modes (concat (repeat (- t (count modes)) 0) modes)]
    (if immediate-last?
      (conj (rest modes) 1)
      modes)))

(defn prepare-args [{:keys [program offset]} modes n immediate-last?]
  (map (fn [mode arg] (if (zero? mode) (nth program arg) arg))
       (reverse (padded-modes modes n immediate-last?))
       (subvec program (inc offset) (+ 1 n offset))))

(defn async-cmd [arg-count last-arg-immediate? op-fn]
  (fn [program modes]
    (-> (update program :offset + 1 arg-count)  ; number of args + opcode
        (op-fn (prepare-args program modes arg-count last-arg-immediate?)))))

(defn cmd [arg-count last-arg-immediate? op-fn]
  (fn [program modes]
    (go
      (-> (update program :offset + 1 arg-count)  ; number of args + opcode
          (op-fn (prepare-args program modes arg-count last-arg-immediate?))))))

(defn simple-op [op]
  (cmd 3 true (fn [p [a b res]] (assoc-in p [:program res] (op a b)))))

(def input-op
  (async-cmd 1 true (fn [p [res]]
                      (go
                        (let [input (<! (:input p))]
                          (assoc-in p [:program res] input))))))

(def output-op
  (async-cmd 1 false (fn [p [o]]
                       (go
                         (put! (:output p) o)
                         p))))

(defn cond-jump-op [pred]
  (cmd 2 false (fn [p [t res]] (if (pred t) (assoc p :offset res) p))))

(defn compare-op [cmp]
  (cmd 3 true (fn [p [a b res]]
                (assoc-in p [:program res] (if (cmp a b) 1 0)))))

(def halt-op (cmd 0 false (fn [p _] (assoc p :halt true))))

(def operations
  {1 (simple-op +)
   2 (simple-op *)
   3 input-op
   4 output-op
   5 (cond-jump-op (complement zero?))
   6 (cond-jump-op zero?)
   7 (compare-op <)
   8 (compare-op =)
   99 halt-op})

(defn make-input-channel [inputs]
  (let [input-ch (async/chan)]
    ; XXX: to-chan auto-closes the chan.
    (go (async/onto-chan input-ch inputs false))
    input-ch))

(defn run-program-async
  "Runs the intcode computer with the given input and a map of known commands."
  [program input-ch output-ch]
  (async/go-loop [program {:program program
                           :offset 0
                           :output output-ch
                           :input input-ch}]
    (let [[op modes] (parse-opcode program)
          program (<! ((get operations op) program modes))]
      (if (:halt program) program (recur program)))))

(defn run-program [program inputs]
  (let [res (<!! (run-program-async program
                                    (make-input-channel inputs)
                                    (async/chan)))
        output (:output res)]
    (async/close! output)
    (assoc res :output (<!! (async/reduce conj [] output)))))
