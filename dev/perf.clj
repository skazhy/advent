(ns perf
  "Puzzle sollution performance testing."
  (:require [clojure.java.io :refer [resource]]))

(defmacro measure
  "Usage: (measure dayX/puzzleX dayX/puzzle-input)"
  [[m :as form]]
  `(do
     (println "Measuring" (:name (meta (var ~m))))
     (dorun (repeatedly 5 (fn [] (time ~form))))))
