(ns advent.2020.test-day9
  (:require [clojure.test :refer :all]
            [advent.2020.day9 :as d]))

(def ^:private example [35 20 15 25 47
                        40 62 55 65 95
                        102 117 150 182 127
                        219 299 277 309 576])

(deftest puzzle1
  (testing "Examples"
    (is (= 127 (d/find-missing example 5))))

  (testing "Actual input"
    (is (= 542529149 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 62 (d/break-encryption example 127))))

  (testing "Actual input"
    (is (= 75678618 (d/puzzle2 d/puzzle-input)))))
