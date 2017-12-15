(ns advent.2017.test-day11
  (:require [clojure.test :refer :all]
            [clojure.java.io :refer [resource]]
            [advent.helpers :refer [slurp-line]]
            [advent.2017.day11 :as d]))


(def ^:private day11-input (slurp-line (resource "2017/day11.txt")))

(deftest puzzle1
  (testing "Examples"
    (is (= 3 (d/puzzle1 "ne,ne,ne")))
    (is (= 0 (d/puzzle1 "ne,ne,sw,sw")))
    (is (= 2 (d/puzzle1 "ne,ne,s,s")))
    (is (= 3 (d/puzzle1 "se,sw,se,sw,sw"))))

  (testing "Actual input"
    (is (= 720 (d/puzzle1 day11-input)))))


(deftest puzzle2
  (testing "Examples"
    (is (= 3 (d/puzzle2 "ne,ne,ne")))
    (is (= 2 (d/puzzle2 "ne,ne,sw,sw")))
    (is (= 2 (d/puzzle2 "ne,ne,s,s")))
    (is (= 3 (d/puzzle2 "se,sw,se,sw,sw"))))

  (testing "Actual input"
    (is (= 1485 (d/puzzle2 day11-input)))))
