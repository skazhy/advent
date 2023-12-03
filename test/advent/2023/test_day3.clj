(ns advent.2023.test-day3
  (:require [advent.2023.day3 :as d]
            [clojure.test :refer [deftest is testing]]))

(def ^:private example ["467..114.."
                        "...*......"
                        "..35..633."
                        "......#..."
                        "617*......"
                        ".....+.58."
                        "..592....."
                        "......755."
                        "...$.*...."
                        ".664.598.."])

(deftest puzzle1
  (testing "Examples"
    (is (= 4361 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 537832 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 467835 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 81939900 (d/puzzle2 d/puzzle-input)))))
