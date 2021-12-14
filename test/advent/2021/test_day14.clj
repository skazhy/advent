(ns advent.2021.test-day14
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2021.day14 :as d]))

(def ^:private example ["NNCB"
                        ""
                        "CH -> B"
                        "HH -> N"
                        "CB -> H"
                        "NH -> C"
                        "HB -> C"
                        "HC -> B"
                        "HN -> C"
                        "NN -> C"
                        "BH -> H"
                        "NC -> B"
                        "NB -> B"
                        "BN -> B"
                        "BB -> N"
                        "BC -> B"
                        "CC -> N"
                        "CN -> C"])

(deftest puzzle1
  (testing "Examples"
    (is (= 1588 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 2027 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 2188189693529 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 2265039461737 (d/puzzle2 d/puzzle-input)))))
