(ns advent.2021.day4
  "Advent of Code 2021, day 4: Day 4: Giant Squid"
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2021/day4.txt" h/slurp-lines))

(defn prepare-boards [input]
  (loop [boards (->> (partition 6 (rest input))
                     (map (comp h/parse-int-matrix next)))
         idx 0
         n->board {}
         board->n {}]
    (if-let [b (first boards)]
      (recur (rest boards)
             (inc idx)
             (reduce (fn [acc n]
                       (update acc n (fnil conj #{}) idx))
                     n->board
                     (flatten b))
             ; Create sets for all rows and columns in the board, we don't care
             ; which are which, as long as it's possible to detect that one
             ; was cleared in a board.
             (assoc board->n idx (map set (concat b (apply map list b)))))
      [n->board board->n])))

(defn board-sum [board n]
  (* n (apply + (distinct (apply concat board)))))

(defn play-bingo [bingo-handler-fn input]
  (let [[n->board board->n] (prepare-boards input)]
    (loop [numbers (map h/parse-int (str/split (first input) #","))
           boards board->n]
      (when-let [n (first numbers)]
        (let [res (reduce (fn [boards idx]
                            (let [new-board (map #(disj % n) (get boards idx))]
                              (if (some empty? new-board)
                                (bingo-handler-fn boards new-board idx n)
                                (assoc boards idx new-board))))
                          boards
                          (filter #(get boards %) (get n->board n)))]
          ;; TODO: figure out why reduced? check does not work here.
          (if (int? res)
            res
            (recur (rest numbers) res)))))))

(defn bingo-handler-1 [boards new-board idx n]
  (reduced (board-sum new-board n)))

(defn puzzle1 [input] (play-bingo bingo-handler-1 input))

(defn bingo-handler-2 [boards new-board idx n]
  (if (= 1 (count boards))
    (reduced (board-sum new-board n))
    (dissoc boards idx)))

(defn puzzle2 [input] (play-bingo bingo-handler-2 input))
