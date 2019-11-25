(ns advent.2017.test-day3
  (:require [clojure.test :refer :all]
            [advent.2017.day3 :as d]))

(deftest puzzle1
  (testing "Examples"
    (is (zero? (d/puzzle1 1)))
    (is (= 3 (d/puzzle1 12)))
    (is (= 2 (d/puzzle1 23)))
    (is (= 31 (d/puzzle1 1024))))

  (testing "Actual input"
    (is (= 475 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (is (= 279138 (d/puzzle2 d/puzzle-input))))
