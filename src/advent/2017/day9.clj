(ns advent.2017.day9)


(defn c-gr [items callback-fn]
  (loop [items items in-garbage? false skip? false depth 0 tot-sc 0 gco 0]
    (if-let [i (first items)]
      (cond
        skip? (recur (rest items) in-garbage? false depth tot-sc gco)
        (= \! i) (recur (rest items) in-garbage? true depth tot-sc gco)
        (and (= \> i) in-garbage?) (recur (rest items) false false depth tot-sc gco)
        in-garbage? (recur (rest items) in-garbage? false depth tot-sc (inc gco))
        (= \{ i) (recur (rest items) false false (inc depth) tot-sc gco)
        (= \} i) (recur (rest items) false false (dec depth) (+ tot-sc depth) gco)
        (= \< i) (recur (rest items) true false depth tot-sc gco)
        :else (recur (rest items) in-garbage? false depth tot-sc gco))
      (callback-fn tot-sc gco))))

(defn puzzle1 [items]
  (c-gr items (fn [tot-sc _] tot-sc)))

(defn puzzle2 [items]
  (c-gr items (fn [_ gco] gco)))
