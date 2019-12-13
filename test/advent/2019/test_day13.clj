(ns advent.2019.test-day13
  (:require [clojure.test :refer :all]
            [advent.2019.day13 :as d]))

(deftest puzzle1
  (is (= 296 (d/puzzle1 d/puzzle-input))))
