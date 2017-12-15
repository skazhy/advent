(ns advent.2017.test-day2
  (:require [clojure.test :refer :all]
            [clojure.java.io :refer [resource]]
            [advent.helpers :refer [slurp-int-matrix]]
            [advent.2017.day2 :as d]))


(def ^:private day2-input (slurp-int-matrix (resource "2017/day2.txt")))

(deftest puzzle1
  (testing "Example"
    (is (= 18 (d/puzzle1 [[5 1 9 5] [7 5 3] [2 4 6 8]]))))

  (testing "Actual input"
    (is (= 39126 (d/puzzle1 day2-input)))))


(deftest puzzle2
  (testing "Example"
    (is (= 9 (d/puzzle2 [[5 9 2 8] [9 4 7 3] [3 8 6 5]]))))

  (testing "Actual input"
    (is (= 258 (d/puzzle2 day2-input)))))
