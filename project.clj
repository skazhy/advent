(defproject advent "latest"
  :description "Advent of Code 2017"
  :url "https://github.com/skazhy/advent"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"
            :key "mit"
            :year 2015}
  :resource-paths ["resources"]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/core.async "0.6.532"]]
  :test-selectors {:default (complement :slow)
                   :intcode :intcode}  ; A group of 2019 puzzles
  :aliases {"perf" ["run" "-m" "perf"]}
  :aot :all
  :profiles {:dev {:source-paths ["dev"]}})
