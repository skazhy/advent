(ns advent.2020.test-day1
  (:require [clojure.test :refer :all]
            [advent.2020.day1 :as d]))

(deftest puzzle1
  (testing "Examples"
    (is (= 514579 (d/puzzle1 [1721 979 366 299 675 1456]))))

  (testing "Actual input"
    (is (= 1014624 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 241861950 (d/puzzle2 [1721 979 366 299 675 1456]))))

  (testing "Actual input"
    (is (= 80072256 (d/puzzle2 d/puzzle-input)))))
