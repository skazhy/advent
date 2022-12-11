(ns advent.2022.test-day11
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2022.day11 :as d]))

(def example ["Monkey 0:"
              "  Starting items: 79, 98"
              "  Operation: new = old * 19"
              "  Test: divisible by 23"
              "    If true: throw to monkey 2"
              "    If false: throw to monkey 3"
              ""
              "Monkey 1:"
              "  Starting items: 54, 65, 75, 74"
              "  Operation: new = old + 6"
              "  Test: divisible by 19"
              "    If true: throw to monkey 2"
              "    If false: throw to monkey 0"
              ""
              "Monkey 2:"
              "  Starting items: 79, 60, 97"
              "  Operation: new = old * old"
              "  Test: divisible by 13"
              "    If true: throw to monkey 1"
              "    If false: throw to monkey 3"
              ""
              "Monkey 3:"
              "  Starting items: 74"
              "  Operation: new = old + 3"
              "  Test: divisible by 17"
              "    If true: throw to monkey 0"
              "    If false: throw to monkey 1"])

(deftest puzzle1
  (testing "Examples"
    (is (= 10605 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 113220 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 2713310158 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 30599555965 (d/puzzle2 d/puzzle-input)))))
