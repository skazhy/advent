(defproject advent "latest"
  :description "Advent of Code 2017"
  :url "https://github.com/skazhy/advent"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"
            :key "mit"
            :year 2015}
  :resource-paths ["resources"]
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :aliases {"perf" ["run" "-m" "perf"]}
  :aot :all
  :profiles {:dev {:source-paths ["dev"]}})
