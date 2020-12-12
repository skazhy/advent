(ns advent.2020.test-day12
  (:require [clojure.test :refer :all]
            [advent.2020.day12 :as d]))

(def ^:private example ["F10" "N3" "F7" "R90" "F11"])

(deftest puzzle1
  (testing "Examples"
    (is (= 25 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 923 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 286 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 24769 (d/puzzle2 d/puzzle-input)))))
