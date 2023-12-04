#!/usr/bin/env bb

(require '[clojure.string :as str])
(require '[babashka.http-client :as http])

;;; Data fetch

(defn puzzle-url [year day] (str "https://adventofcode.com/" year "/day/" day))

(defn puzzle-html-lines [year day]
  (let [res (:body (http/get (puzzle-url year day)
                             {:headers {"Cookie" (str/trim (slurp ".cookie"))}}))]
    (str/split res #"\n")))

(def title-re #"--- Day (\d+)?: (.+) ---")

(defn title-http-lookup [year day]
  (->> (puzzle-html-lines year day)
       (filter #(str/starts-with? % "<article"))
       first
       (re-find title-re)
       last))

;;; Caching

(def cache-path  ".aoccache/titles")

(defn title-cache-lookup [year day]
  (with-open [rdr (clojure.java.io/reader cache-path)]
    (when-let [item (->> (line-seq rdr)
                         (filter #(str/starts-with? % (str year day)))
                         first)]
      (last (str/split item #" " 2)))))

(defn cache-write [title year day]
  (spit cache-path (str year day " " title "\n") :append true))

;;; Title lookup

(defn title-lookup [year day]
  (or (title-cache-lookup year day)
      (when-let [http-res (title-http-lookup year day)]
        (cache-write http-res year day)
        http-res)))

;;; Solution lookup

(def code-re #"<code>(\d+)</code>")

(defn solution-lookup [year day]
  (let [solutions (->> (puzzle-html-lines year day)
                       (filter #(str/starts-with? % "<p>Your puzzle answer was <code>"))
                       (map #(last (re-find code-re %))))]
    (spit (str "resources/" year "/solutions/day" day ".txt") (str (str/join "\n" solutions) "\n"))))

(defn -main [[op & args]]
  (case op
    "title" (println (apply title-lookup args))
    "sol" (println (apply solution-lookup args))
    nil))

(-main *command-line-args*)
