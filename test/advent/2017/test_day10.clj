(ns advent.2017.test-day10
  (:require [clojure.test :refer :all]
            [advent.2017.day10 :as d]))


(deftest puzzle1
  (testing "Example"
    (is (= 12 (d/puzzle1 "3,4,1,5" 5))))

  (testing "Actual input"
    (is (= 40132 (d/puzzle1 d/puzzle-input 256)))))


(deftest puzzle2
  (testing "Examples"
    (is (= "a2582a3a0e66e6e86e3812dcb672a272" (d/puzzle2 "")))
    (is (= "33efeb34ea91902bb2f59c9920caa6cd" (d/puzzle2 "AoC 2017")))
    (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (d/puzzle2 "1,2,3")))
    (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (d/puzzle2 "1,2,4"))))

  (testing "Actual input"
    (is (= "35b028fe2c958793f7d5a61d07a008c8" (d/puzzle2 d/puzzle-input)))))
