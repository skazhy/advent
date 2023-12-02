(ns advent.2023.test-day2
  (:require [advent.2023.day2 :as d]
            [clojure.test :refer [deftest is testing]]))

(def ^:private example
  ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(deftest puzzle1
  (testing "Examples"
    (is (= 8 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 2204 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 2286 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 71036 (d/puzzle2 d/puzzle-input)))))
