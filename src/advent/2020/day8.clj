(ns advent.2020.day8
  "Advent of Code 2020, day 8: A Puzzle"
  (:require [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day8.txt" h/slurp-lines))

(defn parse-line [l]
  (let [[_ cmd arg] (re-matches #"([a-z]{3}) ((\+|-)[\d]+)" l)]
    {:cmd (keyword cmd) :arg (Integer/parseInt arg)}))

(defn run-program [input]
  (loop [acc 0 pos 0 commands input]
    (if (<= (count commands) pos)
      {:accumulator acc :loop? false}
      (let [{:keys [cmd arg run?]} (nth commands pos)]
        (if run?
          {:accumulator acc :loop? true}
          (recur (if (= :acc cmd) (+ acc arg) acc)
                 (if (= :jmp cmd) (+ pos arg) (inc pos))
                 (assoc-in commands [pos :run?] true)))))))

(def key-swap {:nop :jmp :jmp :nop})

(defn exited-without-looping? [input]
  (let [{:keys [accumulator loop?]} (run-program input)]
    (when-not loop?
      accumulator)))

(defn puzzle1 [input] (:accumulator (run-program (mapv parse-line input))))

(defn puzzle2 [input]
  (let [parsed (mapv parse-line input)]
    (->> parsed
         (map-indexed (fn [idx {:keys [cmd]}] (when (key-swap cmd) idx)))
         (filter identity)
         (some #(exited-without-looping? (update-in parsed [% :cmd] key-swap))))))
