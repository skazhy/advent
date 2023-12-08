(ns advent.2023.test-day8
  (:require [advent.2023.day8 :as d]
            [clojure.test :refer [deftest is testing]]))

(def example ["RL"
              ""
              "AAA = (BBB, CCC)"
              "BBB = (DDD, EEE)"
              "CCC = (ZZZ, GGG)"
              "DDD = (DDD, DDD)"
              "EEE = (EEE, EEE)"
              "GGG = (GGG, GGG)"
              "ZZZ = (ZZZ, ZZZ)"])

(def example2 ["LLR"
               ""
               "AAA = (BBB, BBB)"
               "BBB = (AAA, ZZZ)"
               "ZZZ = (ZZZ, ZZZ)"])

(def example3 ["LR"
                ""
                "11A = (11B, XXX)"
                "11B = (XXX, 11Z)"
                "11Z = (11B, XXX)"
                "22A = (22B, XXX)"
                "22B = (22C, 22C)"
                "22C = (22Z, 22Z)"
                "22Z = (22B, 22B)"
                "XXX = (XXX, XXX)"])

(deftest puzzle1
  (testing "Examples"
    (is (= 2 (d/puzzle1 example)))
    (is (= 6 (d/puzzle1 example2)))))

(deftest puzzle2
  (testing "Example"
    (is (= 6 (d/puzzle2 example3)))))
