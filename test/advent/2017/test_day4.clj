(ns advent.2017.test-day4
  (:require [clojure.test :refer :all]
            [advent.2017.day4 :as d]))


(deftest puzzle1
  (testing "Examples"
    (is (= 1 (d/puzzle1 [["aa" "bb" "cc" "dd" "ee"]])))
    (is (zero? (d/puzzle1 [["aa" "bb" "cc" "dd" "aa"]])))
    (is (= 1 (d/puzzle1 [["aa" "bb" "cc" "dd" "aaa"]]))))

  (testing "Actual input"
    (is (= 455 (d/puzzle1 d/puzzle-input)))))


(deftest puzzle2
  (testing "Examples"
    (is (= 1 (d/puzzle2 [["abcde" "fghij"]])))
    (is (zero? (d/puzzle2 [["abcde" "xyz" "ecdab"]])))
    (is (= 1 (d/puzzle2 [["a" "ab" "abc" "abd" "abf" "abj"]])))
    (is (= 1 (d/puzzle2 [["iiii" "oiii" "ooii" "oooi" "oooo"]])))
    (is (zero? (d/puzzle2 [["oiii" "ioii" "iioi" "iiio"]]))))

  (testing "Actual input"
    (is (= 186 (d/puzzle2 d/puzzle-input)))))
