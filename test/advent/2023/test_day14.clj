(ns advent.2023.test-day14
  (:require [advent.2023.day14 :as d]
            [clojure.test :refer [deftest is testing]]))

(def ^:private example ["O....#...."
                        "O.OO#....#"
                        ".....##..."
                        "OO.#O....O"
                        ".O.....O#."
                        "O.#..O.#.#"
                        "..O..#O..O"
                        ".......O.."
                        "#....###.."
                        "#OO..#...."])

(deftest puzzle1
  (testing "Example"
    (is (= 136 (d/puzzle1 example)))))

(deftest puzzle2
  (testing "Example"
    (is (= 64 (d/puzzle2 example)))))
