(ns advent.2024.test-day8
  (:require [advent.2024.day8 :as d]
            [clojure.test :refer [deftest is testing]]))

(def example ["............"
              "........0..."
              ".....0......"
              ".......0...."
              "....0......."
              "......A....."
              "............"
              "............"
              "........A..."
              ".........A.."
              "............"
              "............"])

(def example2 ["T........."
               "...T......"
               ".T........"
               ".........."
               ".........."
               ".........."
               ".........."
               ".........."
               ".........."
               ".........."])

(deftest puzzle1
  (testing "Example"
    (is (= 14 (d/puzzle1 example)))))

(deftest puzzle2
  (testing "Example"
    (is (= 34 (d/puzzle2 example))
        (= 9 (d/puzzle2 example2)))))
