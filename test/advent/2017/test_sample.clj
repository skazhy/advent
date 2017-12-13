(ns advent.2017.test-sample
  "Testing provided puzzle samples."
  (:require [clojure.test :refer :all]
            [advent.2017.day1 :as day1]
            [advent.2017.day2 :as day2]
            [advent.2017.day3 :as day3]
            [advent.2017.day4 :as day4]
            [advent.2017.day5 :as day5]
            [advent.2017.day6 :as day6]
            [advent.2017.day7 :as day7]
            [advent.2017.day8 :as day8]
            [advent.2017.day9 :as day9]
            [advent.2017.day10 :as day10]
            [advent.2017.day11 :as day11]
            [advent.2017.day12 :as day12]))


(defn- split-lines [raw-lines]
  (map #(clojure.string/split % #" ") raw-lines))

(deftest day-1-puzzles
  (testing "Day 1, first puzzle"
    (is (= 3 (day1/puzzle1 "1122")))
    (is (= 4 (day1/puzzle1 "1111")))
    (is (= 9 (day1/puzzle1 "91212129"))))

  (testing "Day 1, second puzzle"
    (is (= 6 (day1/puzzle2 "1212")))
    (is (zero? (day1/puzzle2 "1221")))
    (is (= 4 (day1/puzzle2 "123425")))
    (is (= 12 (day1/puzzle2 "123123")))
    (is (= 4 (day1/puzzle2 "12131415")))))


(deftest day-2-puzzles
  (testing "Day 2, first puzzle"
    (is (= 18 (day2/puzzle1 [[5 1 9 5] [7 5 3] [2 4 6 8]]))))

  (testing "Day 2, second puzzle"
    (is (= 9 (day2/puzzle2 [[5 9 2 8] [9 4 7 3] [3 8 6 5]])))))


(deftest day-3-puzzles
  (testing "Day 3, first puzzle"
    (is (zero? (day3/puzzle1 1)))
    (is (= 3 (day3/puzzle1 12)))
    (is (= 2 (day3/puzzle1 23)))
    (is (= 31 (day3/puzzle1 1024))))

  (testing "Day 3, second puzzle"
    (is (= 279138 (day3/puzzle2 277678)))))


(deftest day-4-puzzles
  (testing "Day 4, first puzzle"
    (is (= 1 (day4/puzzle1 [["aa" "bb" "cc" "dd" "ee"]])))
    (is (zero? (day4/puzzle1 [["aa" "bb" "cc" "dd" "aa"]])))
    (is (= 1 (day4/puzzle1 [["aa" "bb" "cc" "dd" "aaa"]]))))

  (testing "Day 4, second puzzle"
    (is (= 1 (day4/puzzle2 [["abcde" "fghij"]])))
    (is (zero? (day4/puzzle2 [["abcde" "xyz" "ecdab"]])))
    (is (= 1 (day4/puzzle2 [["a" "ab" "abc" "abd" "abf" "abj"]])))
    (is (= 1 (day4/puzzle2 [["iiii" "oiii" "ooii" "oooi" "oooo"]])))
    (is (zero? (day4/puzzle2 [["oiii" "ioii" "iioi" "iiio"]])))))


(deftest day-5-puzzles
  (testing "Day 5, first puzzle"
    (is (= 5 (day5/puzzle1 [0 3  0  1  -3]))))

  (testing "Day 5, second puzzle"
    (= 10 (day5/puzzle2 [0 3  0  1  -3]))))


(deftest day-6-puzzles
  (def ^:private day-6-seq [14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4])

  (testing "Day 6, first puzzle"
    (is (= 5 (day6/puzzle1 [0 2 7 0])))
    (is (= 11137 (day6/puzzle1 day-6-seq))))

  (testing "Day 6, second puzzle"
    (is (= 4 (day6/puzzle2 [0 2 7 0])))
    (is (= 1037 (day6/puzzle2 day-6-seq)))))


(deftest day-7-puzzles
  (def ^:private day-7-tree
    (split-lines
      ["pbga (66)""xhth (57)" "ebii (61)" "havc (66)" "ktlj (57)"
       "fwft (72) -> ktlj, cntj, xhth" "qoyq (66)"
       "padx (45) -> pbga, havc, qoyq" "tknk (41) -> ugml, padx, fwft"
       "jptl (61)" "ugml (68) -> gyxo, ebii, jptl" "gyxo (61)" "cntj (57)"]))

  (testing "Day 7, first puzzle"
    (is (= "tknk" (day7/puzzle1 day-7-tree))))

  (testing "Day 7, second puzzle"
    (is (= 60 (day7/puzzle2 day-7-tree)))))


(deftest day-8-puzzles
  (def ^:private day-8-instructions
    (split-lines
      ["b inc 5 if a > 1"
       "a inc 1 if b < 5"
       "c dec -10 if a >= 1"
       "c inc -20 if c == 10"]))

  (testing "Day 8, first puzzle"
    (is (= 1 (day8/puzzle1 day-8-instructions))))

  (testing "Day 8, second puzzle"
    (is (= 10 (day8/puzzle2 day-8-instructions)))))

(deftest day-9-puzzles
  (testing "Day 9, first puzzle"
    (is (= 1 (day9/puzzle1 "{}")))
    (is (= 6 (day9/puzzle1 "{{{}}}")))
    (is (= 5 (day9/puzzle1 "{{},{}}")))
    (is (= 16 (day9/puzzle1 "{{{},{},{{}}}}")))
    (is (= 1 (day9/puzzle1 "{<a>,<a>,<a>,<a>}")))
    (is (= 9 (day9/puzzle1 "{{<ab>},{<ab>},{<ab>},{<ab>}}")))
    (is (= 9 (day9/puzzle1 "{{<!!>},{<!!>},{<!!>},{<!!>}}")))
    (is (= 3 (day9/puzzle1 "{{<a!>},{<a!>},{<a!>},{<ab>}}"))))

  (testing "Day 9, second puzzle"
    (is (= 0 (day9/puzzle2 "<>")))
    (is (= 17 (day9/puzzle2 "<random characters>")))
    (is (= 3 (day9/puzzle2 "<<<<>")))
    (is (= 2 (day9/puzzle2 "<{!>}>")))
    (is (= 0 (day9/puzzle2 "<!!>")))
    (is (= 0 (day9/puzzle2 "<!!!>>")))
    (is (= 10 (day9/puzzle2 "<{o\"i!a,<{i<a>")))))


(deftest day-10-puzzles
  (def ^:private day-10-str
    "197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63")

  (testing "Day 10, first puzzle"
    (is (= 12 (day10/puzzle1 "3,4,1,5" 5)))
    (is (= 40132 (day10/puzzle1 day-10-str 256))))

  (testing "Day 10, second puzzle"
    (is (= "a2582a3a0e66e6e86e3812dcb672a272" (day10/puzzle2 "")))
    (is (= "33efeb34ea91902bb2f59c9920caa6cd" (day10/puzzle2 "AoC 2017")))
    (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (day10/puzzle2 "1,2,3")))
    (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (day10/puzzle2 "1,2,4")))
    (is (= "35b028fe2c958793f7d5a61d07a008c8" (day10/puzzle2 day-10-str)))))


(deftest day-11-puzzles
  (testing "Day 11, first puzzle"
    (is (= 3 (day11/puzzle1 "ne,ne,ne")))
    (is (= 0 (day11/puzzle1 "ne,ne,sw,sw")))
    (is (= 2 (day11/puzzle1 "ne,ne,s,s")))
    (is (= 3 (day11/puzzle1 "se,sw,se,sw,sw"))))

  (testing "Day 11, second puzzle"
    (is (= 3 (day11/puzzle2 "ne,ne,ne")))
    (is (= 2 (day11/puzzle2 "ne,ne,sw,sw")))
    (is (= 2 (day11/puzzle2 "ne,ne,s,s")))
    (is (= 3 (day11/puzzle2 "se,sw,se,sw,sw")))))


(deftest day-12-puzzles
  (def ^:private day-12-rows
    ["0 <-> 2"
     "1 <-> 1"
     "2 <-> 0, 3, 4"
     "3 <-> 2, 4"
     "4 <-> 2, 3, 6"
     "5 <-> 6"
     "6 <-> 4, 5"])

  (testing "Day 12, puzzle 1"
    (is (= 6 (day12/puzzle1 day-12-rows))))

  (testing "Day 12, puzzle 2"
    (is (= 2 (day12/puzzle2 day-12-rows)))))
