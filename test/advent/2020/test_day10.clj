(ns advent.2020.test-day10
  (:require [clojure.test :refer :all]
            [advent.2020.day10 :as d]))

(def ^:private example [16 10 15 5 1 11 7 19 6 12 4])

(def ^:private example2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19
                         38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(deftest puzzle1
  (testing "Examples"
    (is (= 35 (d/puzzle1 example)))
    (is (= 220 (d/puzzle1 example2))))

  (testing "Actual input"
    (is (= 2346 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 8 (d/puzzle2 example)))
    (is (= 19208 (d/puzzle2 example2))))

  (testing "Actual input"
    (is (= 6044831973376 (d/puzzle2 d/puzzle-input)))))
