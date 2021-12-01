(ns advent.2021.test-day1
  (:require [clojure.test :refer :all]
            [advent.2021.day1 :as d]))

(def ^:private example [199 200 208 210 200 207 240 269 260 263])

(deftest puzzle1
  (testing "Examples"
    (is (= 7 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 1502 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 5 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 1538 (d/puzzle2 d/puzzle-input)))))
