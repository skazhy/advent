(ns advent.2017.test-day25
  (:require [clojure.test :refer :all]
            [advent.2017.day25 :as d]))


(def ^:private day25-example
  {:a {0 {:val 1 :op inc :to-state :b} 1 {:val 0 :op dec :to-state :b}}
   :b {0 {:val 1 :op dec :to-state :a} 1 {:val 1 :op inc :to-state :a}}})

(deftest ^:slow puzzle1
  (testing "Example"
    (is (= 3 (d/puzzle1 day25-example 6))))

  (testing "Actual input"
    (is (= 2474 (d/puzzle1 d/puzzle-input 12172063)))))
