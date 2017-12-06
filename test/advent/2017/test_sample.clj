(ns advent.2017.test-sample
  "Testing provided puzzle samples."
  (:require [clojure.test :refer :all]
            [advent.2017.day1 :as day1]
            [advent.2017.day2 :as day2]
            [advent.2017.day3 :as day3]
            [advent.2017.day4 :as day4]
            [advent.2017.day5 :as day5]
            [advent.2017.day6 :as day6]))


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
