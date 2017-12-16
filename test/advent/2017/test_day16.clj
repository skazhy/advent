(ns advent.2017.test-day16
  (:require [clojure.test :refer :all]
            [advent.2017.day16 :as d]))


(deftest puzzle1
  (= "jkmflcgpdbonihea" (d/puzzle1 d/puzzle-input)))

(deftest puzzle2
  (= "ajcdefghpkblmion" (d/puzzle2 d/puzzle-input)))
