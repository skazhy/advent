(ns advent.2023.test-day5
  (:require [advent.2023.day5 :as d]
            [clojure.test :refer [are deftest is]]))

(def example [["seeds: 79 14 55 13"]
              ["seed-to-soil map:"
               "50 98 2"
               "52 50 48"]
              ["soil-to-fertilizer map:"
               "0 15 37"
               "37 52 2"
               "39 0 15"]
              ["fertilizer-to-water map:"
               "49 53 8"
               "0 11 42"
               "42 0 7"
               "57 7 4"]
              ["water-to-light map:"
               "88 18 7"
               "18 25 70"]
              ["light-to-temperature map:"
               "45 77 23"
               "81 45 19"
               "68 64 13"]
              ["temperature-to-humidity map:"
               "0 69 1"
               "1 0 69"]
              ["humidity-to-location map:"
               "60 56 37"
               "56 93 4"]])

(deftest seed-row-parse-test
  (is (= (d/parse-seed-row "seeds: 79 14 55 13")
         [[79 92] [55 67]])))

(deftest range-map-tests
  (are [input found missed] (= (d/map-range [5 10 30] input) [missed found])
    ;; input mapped missed
    [17 19]  [12 14] []
    [1 3]    nil    [[1 3]]
    [40 42]  nil    [[40 42]]
    [9 11]   [5 6]  [[9 9]]
    [28 33]  [23 25] [[31 33]]
    [9 31]   [5 25] [[9 9] [31 31]]))

(deftest puzzle1
  (is (= 35 (d/puzzle1 example))))

(deftest puzzle2
  (is (= 46 (d/puzzle2 example))))
