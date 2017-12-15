(ns advent.2017.test-day6
  (:require [clojure.test :refer :all]
            [clojure.java.io :refer [resource]]
            [advent.helpers :refer [slurp-int-matrix]]
            [advent.2017.day6 :as d]))


(def ^:private day6-input
  (vec (first (slurp-int-matrix (resource "2017/day6.txt")))))

(deftest puzzle1
  (testing "Example"
    (is (= 5 (d/puzzle1 [0 2 7 0]))))

  (testing "Actual input"
    (is (= 11137 (d/puzzle1 day6-input)))))


(deftest puzzle2
  (testing "Example"
    (is (= 4 (d/puzzle2 [0 2 7 0]))))

  (testing "Actual input"
    (is (= 1037 (d/puzzle2 day6-input)))))
