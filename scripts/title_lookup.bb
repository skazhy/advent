#!/usr/bin/env bb

(require '[clojure.string :as str])
(require '[babashka.http-client :as http])

(def cache-path  ".aoccache/titles")

(defn puzzle-url [year day] (str "https://adventofcode.com/" year "/day/" day))

(def title-re #"--- Day (\d+)?: (.+) ---")

(defn cache-lookup [year day]
  (with-open [rdr (clojure.java.io/reader cache-path)]
    (when-let [item (first (filter #(str/starts-with? % (str year day)) (line-seq rdr)))]
      (last (str/split item #" " 2)))))

(defn cache-write [title year day]
  (spit cache-path (str year day " " title "\n") :append true))

(defn http-lookup [year day]
  (let [res (:body (http/get (puzzle-url year day)))]
    (last
     (re-find title-re
              (first (filter #(str/starts-with? % "<article") (str/split res #"\n")))))))

(defn -main [year day]
  (println
   (or (cache-lookup year day)
       (when-let [http-res (http-lookup year day)]
         (cache-write http-res year day)
         http-res))))

(apply -main *command-line-args*)
