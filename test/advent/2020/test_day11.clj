(ns advent.2020.test-day11
  (:require [clojure.test :refer :all]
            [advent.2020.day11 :as d]))

(def ^:private example ["L.LL.LL.LL"
                        "LLLLLLL.LL"
                        "L.L.L..L.."
                        "LLLL.LL.LL"
                        "L.LL.LL.LL"
                        "L.LLLLL.LL"
                        "..L.L....."
                        "LLLLLLLLLL"
                        "L.LLLLLL.L"
                        "L.LLLLL.LL"])

(deftest puzzle1
  (testing "Examples"
    (is (= 37 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 2310 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 26 (d/puzzle2 example))))

  (testing "Actual input"
    (is (= 2074 (d/puzzle2 d/puzzle-input)))))
