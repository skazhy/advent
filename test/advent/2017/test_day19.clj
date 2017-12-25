(ns advent.2017.test-day19
  (:require [clojure.test :refer :all]
            [advent.2017.day19 :as d]))


(def day19-example
  ["     |"
   "     |  +--+"
   "     A  |  C"
   " F---|----E|--+"
   "     |  |  |  D"
   "     +B-+  +--+"])


(deftest puzzle1
  (testing "Example"
    (is (= "ABCDEF" (d/puzzle1 day19-example))))

  (testing "Actual input"
    (is (= "YOHREPXWN" (d/puzzle1 d/puzzle-input)))))


(deftest puzzle2
  (testing "Example"
    (is (= 38 (d/puzzle2 day19-example))))

  (testing "Actual input"
    (is (= 16734 (d/puzzle2 d/puzzle-input)))))
