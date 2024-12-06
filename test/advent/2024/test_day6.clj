(ns advent.2024.test-day6
  (:require [advent.2024.day6 :as d]
            [clojure.test :refer [deftest is testing]]))

(def ^:private example ["....#....."
              ".........#"
              ".........."
              "..#......."
              ".......#.."
              ".........."
              ".#..^....."
              "........#."
              "#........."
              "......#..."])

(deftest puzzle1
  (testing "Example"
    (is (= 41 (d/puzzle1 example)))))

(deftest puzzle2
  (testing "Example"
    (is (= 6 (d/puzzle2 example)))))
