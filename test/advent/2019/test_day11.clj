(ns advent.2019.test-day11
  (:require [clojure.test :refer :all]
            [advent.2019.day11 :as d]))

(deftest puzzle1
  (is (= 1932 (d/puzzle1 d/puzzle-input))))

(deftest puzzle2
  (is (= [" ####  ##  #  # #  #  ##    ## #### ###    "
          " #    #  # #  # # #  #  #    # #    #  #   "
          " ###  #    #### ##   #       # ###  #  #   "
          " #    # ## #  # # #  # ##    # #    ###    "
          " #    #  # #  # # #  #  # #  # #    # #    "
          " ####  ### #  # #  #  ###  ##  #### #  #   "]
         (d/puzzle2 d/puzzle-input))))
