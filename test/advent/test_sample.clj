(ns advent.test-sample
  "Testing provided puzzle samples."
  (:require [clojure.test :refer :all]
            [advent.puzzles :refer :all]))


(deftest day-1-puzzles
  (testing "Day one, first puzzle"
    (is (= 3 (inverse-captcha "1122")))
    (is (= 4 (inverse-captcha "1111")))
    (is (= 9 (inverse-captcha "91212129"))))

  (testing "Day one, second puzzle"
    (is (= 6 (halfway-captcha "1212")))
    (is (zero? (halfway-captcha "1221")))
    (is (= 4 (halfway-captcha "123425")))
    (is (= 12 (halfway-captcha "123123")))
    (is (= 4 (halfway-captcha "12131415")))))


(deftest day-2-puzzles
  (testing "Day two, first puzzle"
    (is (= 18 (corruption-checksum [[5 1 9 5] [7 5 3] [2 4 6 8]]))))

  (testing "Day two, second puzzle"
    (is (= 9 (evenly-divisible-checksum [[5 9 2 8] [9 4 7 3] [3 8 6 5]])))))


(deftest day-3-puzzles
  (testing "Day three, first puzzle"
    (is (zero? (manhattan-distance 1)))
    (is (= 3 (manhattan-distance 12)))
    (is (= 2 (manhattan-distance 23)))
    (is (= 31 (manhattan-distance 1024))))

  (testing "Day three, second puzzle"
    (is (= 279138 (taxicab-neighbor-sum 277678)))))


(deftest day-4-puzzles
  (testing "Day four, first puzzle"
    (is (= 1 (unique-passphrases [["aa" "bb" "cc" "dd" "ee"]])))
    (is (zero? (unique-passphrases [["aa" "bb" "cc" "dd" "aa"]])))
    (is (= 1 (unique-passphrases [["aa" "bb" "cc" "dd" "aaa"]]))))

  (testing "Day four, second puzzle"
    (is (= 1 (no-anagram-passphrases [["abcde" "fghij"]])))
    (is (zero? (no-anagram-passphrases [["abcde" "xyz" "ecdab"]])))
    (is (= 1 (no-anagram-passphrases [["a" "ab" "abc" "abd" "abf" "abj"]])))
    (is (= 1 (no-anagram-passphrases [["iiii" "oiii" "ooii" "oooi" "oooo"]])))
    (is (zero? (no-anagram-passphrases [["oiii" "ioii" "iioi" "iiio"]])))))


(deftest day-5-puzzles
  (testing "Day five, first puzzle"
    (is (= 5 (inc-jumps [0 3  0  1  -3]))))

  (testing "Day five, second puzzle"
    (= 10 (inc-dec-jumps [0 3  0  1  -3]))))
