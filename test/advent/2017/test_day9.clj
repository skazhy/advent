(ns advent.2017.test-day9
  (:require [clojure.test :refer :all]
            [clojure.java.io :refer [resource]]
            [advent.helpers :refer [slurp-line]]
            [advent.2017.day9 :as d]))


(def ^:private day9-input (slurp-line (resource "2017/day9.txt")))

(deftest puzzle1
  (testing "Examples"
    (is (= 1 (d/puzzle1 "{}")))
    (is (= 6 (d/puzzle1 "{{{}}}")))
    (is (= 5 (d/puzzle1 "{{},{}}")))
    (is (= 16 (d/puzzle1 "{{{},{},{{}}}}")))
    (is (= 1 (d/puzzle1 "{<a>,<a>,<a>,<a>}")))
    (is (= 9 (d/puzzle1 "{{<ab>},{<ab>},{<ab>},{<ab>}}")))
    (is (= 9 (d/puzzle1 "{{<!!>},{<!!>},{<!!>},{<!!>}}")))
    (is (= 3 (d/puzzle1 "{{<a!>},{<a!>},{<a!>},{<ab>}}"))))

  (testing "Actual input"
    (is (= 15922 (d/puzzle1 day9-input)))))


(deftest puzzle2
  (testing "Examples"
    (is (= 0 (d/puzzle2 "<>")))
    (is (= 17 (d/puzzle2 "<random characters>")))
    (is (= 3 (d/puzzle2 "<<<<>")))
    (is (= 2 (d/puzzle2 "<{!>}>")))
    (is (= 0 (d/puzzle2 "<!!>")))
    (is (= 0 (d/puzzle2 "<!!!>>")))
    (is (= 10 (d/puzzle2 "<{o\"i!a,<{i<a>"))))

  (testing "Actual input"
    (is (= 7314 (d/puzzle2 day9-input)))))
