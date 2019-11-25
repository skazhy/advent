(ns advent.2017.test-day24
  (:require [clojure.test :refer :all]
            [advent.2017.day24 :as d]))

(def ^:private day24-example
  ["0/2"
   "2/2"
   "2/3"
   "3/4"
   "3/5"
   "0/1"
   "10/1"
   "9/10"])

(deftest puzzle1
  (testing "Example"
    (is (= 31 (d/puzzle1 day24-example))))

  (testing "Actual input"
    (is (= 1868 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Example"
    (is (= 19 (d/puzzle2 day24-example))))

  (testing "Actual input"
    (is (= 1841 (d/puzzle2 d/puzzle-input)))))
