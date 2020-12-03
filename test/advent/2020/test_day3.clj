(ns advent.2020.test-day3
  (:require [clojure.test :refer :all]
            [advent.2020.day3 :as d]))

(def ^:private example ["..##......."
                        "#...#...#.."
                        ".#....#..#."
                        "..#.#...#.#"
                        ".#...##..#."
                        "..#.##....."
                        ".#.#.#....#"
                        ".#........#"
                        "#.##...#..."
                        "#...##....#"
                        ".#..#...#.#"])

(deftest puzzle1
  (testing "Examples"
    (is (= 7 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 254 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 336 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 1666768320 (d/puzzle2 d/puzzle-input)))))
