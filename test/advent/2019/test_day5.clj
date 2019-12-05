(ns advent.2019.test-day5
  (:require [clojure.test :refer :all]
            [advent.2019.day5 :as d]))

(deftest puzzle1
  (is (= 4601506 (d/puzzle1 d/puzzle-input))))

(deftest puzzle2
  (is (= 5525561 (d/puzzle2 d/puzzle-input))))
