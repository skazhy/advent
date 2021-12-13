(ns advent.2021.test-day13
  (:require [clojure.test :refer [deftest is testing]]
            [advent.2021.day13 :as d]))

(def ^:private example
  ["6,10"
   "0,14"
   "9,10"
   "0,3"
   "10,4"
   "4,11"
   "6,0"
   "6,12"
   "4,1"
   "0,13"
   "10,12"
   "3,4"
   "3,0"
   "8,4"
   "1,10"
   "2,14"
   "8,10"
   "9,0"
   ""
   "fold along y=7"
   "fold along x=5"])

(deftest puzzle1
  (testing "Examples"
    (is (= 17 (d/puzzle1 example))))

  (testing "Actual input"
    (is (= 661 (d/puzzle1 d/puzzle-input)))))
