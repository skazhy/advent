(ns advent.2020.day18
  "Advent of Code 2020, day 18: Operation Order"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day18.txt" h/slurp-lines))

(defn add-operation [xp op precendence]
  (cond
    ; first operation in this expression
    (not (get precendence (first xp))) (conj xp op)
    ; last operation needs to be executed first.
    (< (get precendence (first xp)) (get precendence op))
    (concat (take 2 xp) (list (list op (last xp))))
    ; first operation can be executed first
    :else (list op (eval xp))))

(defn append [l i] (concat l [i]))

(defn add-operand [xp op]
  (cond
    (empty? xp) (list op)
    ; append operand to the higher precendence expression
    (list? (last xp)) (append (butlast xp) (eval (append (last xp) op)))
    :else (append xp op)))

(def ops {\+ + \* *})

(defn eval-xp [xp precendence]
  (loop [xp xp state {:stack '() :op '()}]
    (if-let [x (first xp)]
      (recur (rest xp)
       (case x
         \space state
         (\+ \*) (update state :op add-operation (ops x) precendence)
         \( (-> (update state :stack conj (:op state))
                (assoc :op '()))
         \) (-> (update state :op #(add-operand (first (:stack state)) (eval %)))
                (assoc :stack (pop (:stack state))))
         (update state :op add-operand (Integer/parseInt (str x)))))
      (eval (:op state)))))

(defn puzzle1 [input]
  (apply + (map #(eval-xp % {+ 0 * 0}) input)))

(defn puzzle2 [input]
  (apply + (map #(eval-xp % {+ 1 * 0}) input)))
