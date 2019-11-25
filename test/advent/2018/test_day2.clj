(ns advent.2018.test-day2
  (:require [clojure.test :refer :all]
            [advent.2018.day2 :as d]))

(deftest puzzle1
  (testing "Example"
    (is (= 12 (d/puzzle1 ["abcdef" "bababc" "abbcde"
                          "abcccd" "aabcdd" "abcdee" "ababab"]))))

  (testing "Actual input"
    (is (= 5166 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Example"
    (is (= "fgij" (d/puzzle2 ["abcde" "fghij" "klmno"
                              "pqrst" "fguij" "axcye" "wvxyz"]))))

  (testing "Actual input"
    (is (= "cypueihajytordkgzxfqplbwn" (d/puzzle2 d/puzzle-input)))))
