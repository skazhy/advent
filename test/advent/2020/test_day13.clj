(ns advent.2020.test-day13
  (:require [clojure.test :refer :all]
            [advent.2020.day13 :as d]))

(def example ["939" "7,13,x,x,59,x,31,19"])

(deftest puzzle1
  (testing "Examples"
    (is (= 295 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 4938 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 1068781 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 230903629977901 (d/puzzle2 d/puzzle-input)))))
