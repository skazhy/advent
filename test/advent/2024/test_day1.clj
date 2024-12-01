(ns advent.2024.test-day1
  (:require [advent.2024.day1 :as d]
            [advent.helpers :refer [parse-int-matrix]]
            [clojure.test :refer [deftest is testing]]))


(def ^:private example ["3   4"
                        "4   3"
                        "2   5"
                        "1   3"
                        "3   9"
                        "3   3"])

(deftest puzzle1
  (testing "Example"
    (is (= 11 (d/puzzle1 (parse-int-matrix example))))))

(deftest puzzle2
  (testing "Example"
    (is (= 31 (d/puzzle2 (parse-int-matrix example))))))
