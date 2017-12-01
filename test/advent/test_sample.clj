(ns advent.test-sample
  "Testing provided puzzle samples."
  (:require [clojure.test :refer :all]
            [advent.puzzles :refer :all]))


(deftest day-1-puzzles
  (testing "Day one, first puzzle"
    (is (= 3 (inverse-captcha "1122")))
    (is (= 4 (inverse-captcha "1111")))
    (is (= 9 (inverse-captcha "91212129"))))

  (testing "Day one, second puzzle"
    (is (= 6 (halfway-captcha "1212")))
    (is (zero? (halfway-captcha "1221")))
    (is (= 4 (halfway-captcha "123425")))
    (is (= 12 (halfway-captcha "123123")))
    (is (= 4 (halfway-captcha "12131415")))))
