(ns advent.2023.test-day13
  (:require [advent.2023.day13 :as d]
            [clojure.test :refer [deftest is testing]]))

(def ^:private example ["#.##..##."
                        "..#.##.#."
                        "##......#"
                        "##......#"
                        "..#.##.#."
                        "..##..##."
                        "#.#.##.#."
                        ""
                        "#...##..#"
                        "#....#..#"
                        "..##..###"
                        "#####.##."
                        "#####.##."
                        "..##..###"
                        "#....#..#"])
(deftest puzzle1
  (testing "Example"
    (is (= 405 (d/puzzle1 example)))))

(deftest puzzle2
  (testing "Example"
    (is (= 400 (d/puzzle2 example)))))
