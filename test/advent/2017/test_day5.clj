(ns advent.2017.test-day5
  (:require [clojure.test :refer :all]
            [clojure.java.io :refer [resource]]
            [advent.helpers :refer [slurp-int-lines]]
            [advent.2017.day5 :as d]))


(def ^:private day5-input (vec (slurp-int-lines (resource "2017/day5.txt"))))

(deftest puzzle1
  (testing "Example"
    (is (= 5 (d/puzzle1 [0 3 0 1 -3]))))

  (testing "Actual input"
    (is (= 364539 (d/puzzle1 day5-input)))))


(deftest puzzle2
  (testing "Example"
    (= 10 (d/puzzle2 [0 3 0 1 -3])))

  (testing "Actual input"
    (= 27477714 (d/puzzle2 day5-input))))
