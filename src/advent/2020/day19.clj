(ns advent.2020.day19
  "Advent of Code 2020, day 19: Monster Messages"
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2020/day19.txt" h/slurp-lines))

(def rule-re #"(\d+): (\"([a-z])\"|([\d+\|\s]+))")

(defn parse-deps [deps]
  (->> (str/split (str/trim deps) #" ")
       (map #(Integer/parseInt %))))

(defn parse-rule [rule]
  (let [[_ id _ re deps] (re-matches rule-re rule)]
    (cond-> {:id (Integer/parseInt id)}
      re (assoc :re re)
      deps (assoc :deps (map parse-deps (str/split deps #"\|"))))))

(defn with-regex [deps parsed]
  (map (comp :re parsed) deps))

(defn new-known-rule [known]
  (fn [new-known id {:keys [deps]}]
    (let [[head tail] (map #(with-regex % known) deps)]
      (if (and (every? some? head) (every? some? tail))
        (let [re (cond-> (str "(" (str/join "" head))
                   tail  (str "|" (str/join "" tail))
                   true (str ")"))]
          (assoc new-known id {:id id :re re}))
        new-known))))

(defn parse-rules [rules]
  (let [rules (map parse-rule rules)
        known (apply hash-map (mapcat (juxt :id identity) (filter :re rules)))
        unknown (apply hash-map (mapcat (juxt :id identity) (remove :re rules)))]
    ;; known contains rules that have regexes, unknown has rules that depend
    ;; on rules that don't have regexes. in each loop we find unknown rules with known
    ;; dependencies and move them to known.
    (loop [known known unknown unknown]
      (if (= (count rules) (count known))
        (apply hash-map (mapcat (juxt key (comp re-pattern :re val)) known))
        (let [new-known (reduce-kv (new-known-rule known) {} unknown)]
          (recur (merge known new-known)
                 (apply dissoc unknown (keys new-known))))))))

(defn split-input [input]
  [(take-while #(not= "" %) input) (rest (drop-while #(not= "" %) input))])

(defn puzzle1 [input]
  (let [[rules input] (split-input input)
        rules (parse-rules rules)]
    (count (filter #(re-matches (get rules 0) %) input))))

;; Part two using Instaparse

(def replacements
  {"8" "8: 42 | 42 8"
   "11" "11: 42 31 | 42 11 31"})

(defn puzzle2 [input]
  (let [[rules input] (split-input input)
        rules (->> rules
                   (map #(let [[idx _] (str/split % #": ")]
                           (if-let [r (get replacements idx)] r %)))
                   (str/join "\n"))
        ;; Create a new parser by setting 0 as the start rule.
        parser (insta/parser rules :start :0)]
    (->> (map parser input) (remove :index) count)))
