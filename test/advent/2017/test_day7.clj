(ns advent.2017.test-day7
  (:require [clojure.test :refer :all]
            [clojure.java.io :refer [resource]]
            [advent.helpers :refer [slurp-word-lines split-lines]]
            [advent.2017.day7 :as d]))


(def ^:private day7-example
  (split-lines
    ["pbga (66)""xhth (57)" "ebii (61)" "havc (66)" "ktlj (57)"
     "fwft (72) -> ktlj, cntj, xhth" "qoyq (66)"
     "padx (45) -> pbga, havc, qoyq" "tknk (41) -> ugml, padx, fwft"
     "jptl (61)" "ugml (68) -> gyxo, ebii, jptl" "gyxo (61)" "cntj (57)"]))

(def ^:private day7-input (slurp-word-lines (resource "2017/day7.txt")))

(deftest puzzle1
  (testing "Example"
    (is (= "tknk" (d/puzzle1 day7-example))))

  (testing "Actual input"
    (is (= "eqgvf" (d/puzzle1 day7-input)))))


(deftest puzzle2
  (testing "Example"
    (is (= 60 (d/puzzle2 day7-example))))

  (testing "Actual input"
    (is (= 757 (d/puzzle2 day7-input)))))
