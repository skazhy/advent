(ns advent.2023.test-day9
  (:require [advent.2023.day9 :as d]
            [clojure.test :refer [deftest is testing]]))

(def ^:private example [[0 3 6 9 12 15]
                        [1 3 6 10 15 21]
                        [10 13 16 21 30 45]])

(deftest puzzle1
  (testing "Example"
    (is (= 114 (d/puzzle1 example)))))

(deftest puzzle2
  (testing "Example"
    (is (= 2 (d/puzzle2 example)))))
