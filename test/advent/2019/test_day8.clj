 (ns advent.2019.test-day8
   (:require [clojure.test :refer :all]
             [advent.2019.day8 :as d]))

(deftest puzzle1
  (is (= 2440 (d/puzzle1 d/puzzle-input))))

(deftest puzzle2
  (is (= [" XX  XXXX  XX    XX  XX  "
          "X  X    X X  X    X X  X "
          "X  X   X  X       X X    "
          "XXXX  X   X       X X    "
          "X  X X    X  X X  X X  X "
          "X  X XXXX  XX   XX   XX  "]
         (d/puzzle2 d/puzzle-input))))
