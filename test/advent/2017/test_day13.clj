(ns advent.2017.test-day13
  (:require [clojure.test :refer :all]
            [advent.2017.day13 :as d]))

(def ^:private day13-example
  ["0: 3"
   "1: 2"
   "4: 4"
   "6: 4"])

(deftest puzzle1
  (testing "Example"
    (is (= 24 (d/puzzle1 day13-example))))

  (testing "Actual input"
    (is (= 1904 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Example"
    (is (= 10 (d/puzzle2 day13-example))))

  (testing "Actual input"
    (is (= 3833504 (d/puzzle2 d/puzzle-input)))))
