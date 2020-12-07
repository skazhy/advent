(ns advent.2020.test-day7
  (:require [clojure.test :refer :all]
            [advent.2020.day7 :as d]))


(def ^:private example
  ["light red bags contain 1 bright white bag, 2 muted yellow bags."
   "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
   "bright white bags contain 1 shiny gold bag."
   "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
   "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
   "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
   "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
   "faded blue bags contain no other bags."
   "dotted black bags contain no other bags."])

(def ^:private example2 ["shiny gold bags contain 2 dark red bags."
                         "dark red bags contain 2 dark orange bags."
                         "dark orange bags contain 2 dark yellow bags."
                         "dark yellow bags contain 2 dark green bags."
                         "dark green bags contain 2 dark blue bags."
                         "dark blue bags contain 2 dark violet bags."
                         "dark violet bags contain no other bags."])

(deftest puzzle1
  (testing "Examples"
    (is (= 4 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 185 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 32 (d/puzzle2 example)))
    (is (= 126 (d/puzzle2 example2))))

  (testing "Actual input"
    (is (= 89084 (d/puzzle2 d/puzzle-input)))))
