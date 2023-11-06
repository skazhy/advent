(ns advent.2016.test-day6
  (:require [advent.2016.day6 :as d]
            [clojure.test :refer [deftest is testing]]))

(def ^:private example ["eedadn"
                        "drvtee"
                        "eandsr"
                        "raavrd"
                        "atevrs"
                        "tsrnev"
                        "sdttsa"
                        "rasrtv"
                        "nssdts"
                        "ntnada"
                        "svetve"
                        "tesnvt"
                        "vntsnd"
                        "vrdear"
                        "dvrsen"
                        "enarar"])

(deftest puzzle1
  (testing "Examples"
    (is (= "easter" (d/puzzle1 example))))

  (testing "Actual input"
    (is (= "liwvqppc" (d/puzzle1 d/puzzle-input)))))

(deftest puzzle2
  (testing "Examples"
    (is (= "advent" (d/puzzle2 example))))

  (testing "Actual input"
    (is (= "caqfbzlh" (d/puzzle2 d/puzzle-input)))))
