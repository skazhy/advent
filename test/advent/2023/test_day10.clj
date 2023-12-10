(ns advent.2023.test-day10
  (:require [advent.2023.day10 :as d]
            [clojure.test :refer [deftest is testing]]))

(def example ["7-F7-"
              ".FJ|7"
              "SJLL7"
              "|F--J"
              "LJ.LJ"])

(def example2 ["....."
               ".S-7."
               ".|.|."
               ".L-J."
               "....."])

(deftest puzzle1
  (testing "Examples"
    (is (= 8 (d/puzzle1 example)))
    (is (= 4 (d/puzzle1 example2)))))
