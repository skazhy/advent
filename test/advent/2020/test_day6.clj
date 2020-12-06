(ns advent.2020.test-day6
  (:require [clojure.test :refer :all]
            [advent.2020.day6 :as d]))


(def ^:private example ["abc"
                        ""
                        "a" "b" "c"
                        ""
                        "ab" "ac"
                        ""
                        "a" "a" "a" "a"
                        ""
                        "b"])

(deftest puzzle1
  (testing "Examples"
    (is (= 11 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 6443 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 6 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 3232 (d/puzzle2 d/puzzle-input)))))
