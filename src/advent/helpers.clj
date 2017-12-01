(ns advent.helpers)


(defn digit-seq
  "Takes number as a string and returns a sequence with all digits as integers."
  [captcha]
  (map #(Integer. (str %)) captcha))

(defn reduce-indexed
  [f acc items]
  (first
    (reduce
      (fn [[acc idx] item] [(f idx acc item) (inc idx)])
      [acc 0]
      items)))
