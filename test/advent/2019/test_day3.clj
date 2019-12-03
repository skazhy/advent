(ns advent.2019.test-day3
  (:require [clojure.test :refer :all]
            [advent.2019.day3 :as d]))

(deftest puzzle1
  (testing "Examples"
    (is (= 6 (d/puzzle1 [["R8" "U5" "L5" "D3"] ["U7" "R6" "D4" "L4"]])))

    (is (= 159 (d/puzzle1 [["R75" "D30" "R83" "U83" "L12" "D49"
                            "R71" "U7" "L72"]
                           ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"]])))

    (is (= 135 (d/puzzle1 [["R98" "U47" "R26" "D63" "R33" "U87"
                            "L62" "D20" "R33" "U53" "R51"]
                           ["U98" "R91" "D20" "R16" "D67"
                            "R40" "U7" "R15" "U6" "R7"]]))))

  (testing "Actual input"
    (is (= 258 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 30 (d/puzzle2 [["R8" "U5" "L5" "D3"] ["U7" "R6" "D4" "L4"]])))

    (is (= 610 (d/puzzle2 [["R75" "D30" "R83" "U83" "L12" "D49"
                            "R71" "U7" "L72"]
                           ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"]])))

    (is (= 410 (d/puzzle2 [["R98" "U47" "R26" "D63" "R33" "U87"
                            "L62" "D20" "R33" "U53" "R51"]
                           ["U98" "R91" "D20" "R16" "D67"
                            "R40" "U7" "R15" "U6" "R7"]]))))

  (testing "Actual input"
    (is (= 12304 (d/puzzle2 d/puzzle-input)))))
