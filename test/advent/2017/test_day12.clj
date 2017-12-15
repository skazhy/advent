(ns advent.2017.test-day12
  (:require [clojure.test :refer :all]
            [clojure.java.io :refer [resource]]
            [advent.helpers :refer [slurp-lines]]
            [advent.2017.day12 :as d]))


(def ^:private day12-example
  ["0 <-> 2"
   "1 <-> 1"
   "2 <-> 0, 3, 4"
   "3 <-> 2, 4"
   "4 <-> 2, 3, 6"
   "5 <-> 6"
   "6 <-> 4, 5"])

(def ^:private day12-input (slurp-lines (resource "2017/day12.txt")))

(deftest puzzle1
  (testing "Example"
    (is (= 6 (d/puzzle1 day12-example))))

  (testing "Actual input"
    (is (= 152 (d/puzzle1 day12-input)))))


(deftest puzzle2
  (testing "Example"
    (is (= 2 (d/puzzle2 day12-example))))

  (testing "Actual input"
    (is (= 186 (d/puzzle2 day12-input)))))
