#!/usr/bin/env bb

(require '[babashka.fs :as fs])
(require '[babashka.http-client :as http])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

;;; Data fetch

(def cookie-path ".cookie")

(defn read-cookie
  ([] (read-cookie "Invalid cookie value."))
  ([prompt-prefix]
   (println prompt-prefix "Please paste session cookie:")
   (let [cookie (as-> (str/trim (read-line)) c
                  (if (str/starts-with? c "session=") c (str "session=" c)))]
     (if (re-matches #"^session=[0-9a-f]{128}$" cookie)
       (do
         (spit cookie-path cookie)
         cookie)
       (read-cookie "Invalid cookie value. It should be 'session=[a-f0-9]{128}'.")))))

(defn get-cookie []
  (if (fs/exists? cookie-path)
    (str/trim (slurp cookie-path))
    (read-cookie "No cookie found.")))

(defn aoc-req [uri {:keys [auth? method form-params]}]
  (let [{:keys [body status]} (try (http/request (cond-> {:throw false
                                                          :method :get
                                                          :uri uri}
                                                   method (assoc :method method)
                                                   form-params (assoc :form-params form-params)
                                                   auth? (assoc :headers {"Cookie" (get-cookie)})))
                                   (catch Exception e
                                     (println (.getMessage e))
                                     {:status 1}))]
    (if (= 200 status)
      body
      (do
        (println body)
        (read-cookie)
        (aoc-req uri {:auth? auth?})))))

(defn year-url [year] (str "https://adventofcode.com/" year))
(defn puzzle-url [year day] (str (year-url year) "/day/" day))

(defn puzzle-html-lines [year day http-opts]
  (let [res (aoc-req (puzzle-url year day) http-opts)]
    (str/split res #"\n")))

(def title-re #"--- Day (\d+)?: (.+) ---")

(defn title-http-lookup [year day]
  (->> (puzzle-html-lines year day {:auth? false})
       (filter #(str/starts-with? % "<article"))
       first
       (re-find title-re)
       last))

;;; Caching

(def cache-path  "resources/titles")

(defn title-cache-lookup [year day]
  (when (fs/exists? cache-path)
    (with-open [rdr (io/reader cache-path)]
      (when-let [item (->> (line-seq rdr)
                           (filter #(str/starts-with? % (str year day)))
                           first)]
        (last (str/split item #" " 2))))))

(defn cache-write [title year day]
  (spit cache-path (str year day " " title "\n") :append true))

;;; Title lookup

(defn title-lookup [year day]
  (or (title-cache-lookup year day)
      (when-let [http-res (title-http-lookup year day)]
        (cache-write http-res year day)
        http-res)))

;;; Fetch input

(defn input-lookup [year day]
  (->> (aoc-req (str (puzzle-url year day) "/input") {:auth? true})
       (spit (str "resources/" year "/day" day ".txt"))))

;;; Solution lookup

(defn solution-path [year day] (str "resources/" year "/solutions/day" day ".txt"))

(def code-re #"<code>(\d+)</code>")

(defn solution-lookup [year day]
  (let [solutions (->> (puzzle-html-lines year day {:auth? true})
                       (filter #(str/starts-with? % "<p>Your puzzle answer was <code>"))
                       (map #(last (re-find code-re %))))]
    (spit (solution-path year day) (str (str/join "\n" solutions) "\n"))))

;;; Year completion

(defn day-completion [completion-class]
  (case (when completion-class (str/trim completion-class))
    "calendar-verycomplete" 2
    "calendar-complete" 1
    0))

(defn year-completion [year]
  (->> (str/split (aoc-req (year-url year) {:auth? true}) #"\n")
       (filter #(str/includes? % "class=\"calendar-day"))
       (keep #(re-find #"class=\"calendar-day(\d+)( [\w-]+)?\"" %))
       (mapv (fn [[_ day completion]] [day (day-completion completion)]))))

(defn -main [[op & args]]
  (case op
    "title" (println (apply title-lookup args))
    "sol" (apply solution-lookup args)
    "input" (apply input-lookup args)
    "stars" (println (apply year-completion args))
    nil))

(-main *command-line-args*)
