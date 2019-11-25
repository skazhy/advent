(ns advent.2017.test-day1
  (:require [clojure.test :refer :all]
            [advent.2017.day1 :as d]))

(deftest puzzle1
  (testing "Examples"
    (is (= 3 (d/puzzle1 "1122")))
    (is (= 4 (d/puzzle1 "1111")))
    (is (= 9 (d/puzzle1 "91212129"))))

  (testing "Actual input"
    (is (= 1069 (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= 6 (d/puzzle2 "1212")))
    (is (zero? (d/puzzle2 "1221")))
    (is (= 4 (d/puzzle2 "123425")))
    (is (= 12 (d/puzzle2 "123123")))
    (is (= 4 (d/puzzle2 "12131415"))))

  (testing "Actual input"
    (is (= 1268 (d/puzzle2 d/puzzle-input)))))
