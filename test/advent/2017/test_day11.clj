(ns advent.2017.test-day11
  (:require [clojure.test :refer :all]
            [advent.2017.day11 :as d]))


(deftest puzzle1
  (testing "Examples"
    (is (= 3 (d/puzzle1 "ne,ne,ne")))
    (is (= 0 (d/puzzle1 "ne,ne,sw,sw")))
    (is (= 2 (d/puzzle1 "ne,ne,s,s")))
    (is (= 3 (d/puzzle1 "se,sw,se,sw,sw"))))

  (testing "Actual input"
    (is (= 720 (d/puzzle1 d/puzzle-input)))))


(deftest puzzle2
  (testing "Examples"
    (is (= 3 (d/puzzle2 "ne,ne,ne")))
    (is (= 2 (d/puzzle2 "ne,ne,sw,sw")))
    (is (= 2 (d/puzzle2 "ne,ne,s,s")))
    (is (= 3 (d/puzzle2 "se,sw,se,sw,sw"))))

  (testing "Actual input"
    (is (= 1485 (d/puzzle2 d/puzzle-input)))))
