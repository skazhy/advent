(ns advent.2019.test-day6
  (:require [clojure.test :refer :all]
            [advent.2019.day6 :as d]))

(def ^:private example
  ["COM)B"
   "B)C"
   "C)D"
   "D)E"
   "E)F"
   "B)G"
   "G)H"
   "D)I"
   "E)J"
   "J)K"
   "K)L"])

(def ^:private traverse-example
  ["COM)B"
   "B)C"
   "C)D"
   "D)E"
   "E)F"
   "B)G"
   "G)H"
   "D)I"
   "E)J"
   "J)K"
   "K)L"
   "K)YOU"
   "I)SAN"])

(deftest puzzle1
  (testing "Examples"
    (is (= 42 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 162816 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 4 (d/puzzle2 traverse-example))))

  (testing "Actual input"
    (is (= 304 (d/puzzle2 d/puzzle-input)))))
