(ns advent.2017.day7
  (:require [clojure.string :as str]
            [advent.helpers :as h]))

(def puzzle-input (h/slurp-resource "2017/day7.txt" h/slurp-word-lines))

(defn- parse-tree-row
  "Returns a tuple with node name & metadata (weight & children)"
  [words]
  [(first words)
   {:weight (->> (second words) (re-find #"\((\d+)\)") second
                 (Integer/parseInt))
    :children (->> (drop 3 words)
                   (map #(str/replace % "," ""))
                   (seq))}])

(defn- make-tree-map
  "Returns tree as a name to meta mapping."
  [rows]
  (apply hash-map (mapcat parse-tree-row rows)))


;;;


(defn- root-node [tree]
  (let [all-children (set (mapcat (comp :children second) tree))]
    (ffirst (remove (comp all-children first) tree))))

(defn puzzle1
  "Finds the root node name."
  [rows]
  (root-node (make-tree-map rows)))


;;;


(defn subtower-weight
  "Returns the sum of weights of all children + the node itself."
  [tree node-name]
  (let [node (get tree node-name)]
    (->> (map #(subtower-weight tree %) (:children node))
         (apply + (:weight node)))))

(defn children-weights
  "Returns a map of [node-name, weight] for all children of a given node."
  [tree node-name]
  (map (juxt identity #(subtower-weight tree %))
       (:children (get tree node-name))))

(defn unbalanced-node
  "Returns name of the only unbalanced child node (if any)."
  [tree entry]
  (->> (children-weights tree entry)
       (group-by second)    ; group by weights
       ; Extract name from the node that is the only one for given weight.
       (filter (comp #(= 1 %) count second)) first second ffirst))

(defn balanced-weight [tree entry]
  (let [unbalanced (unbalanced-node tree entry)
        result-weight (->> (get tree entry) :children
                           (map (juxt identity #(subtower-weight tree %)))
                           (remove #(= unbalanced (first %))) first second)]
    (->> (map second (children-weights tree unbalanced))
         (apply +)
         (- result-weight))))

(defn- rebalance-node
  "Returns new weight for node to rebalance."
  [tree entry]
  (loop [entry entry parent nil]
    (if-let [unbalanced (unbalanced-node tree entry)]
      (recur unbalanced entry)
      ; If all nodes are balanced - find the correct weight for node parents.
      (balanced-weight tree parent))))

(defn puzzle2 [rows]
  (let [tree (make-tree-map rows)
        entry-node (root-node tree)]
    (rebalance-node tree entry-node)))
