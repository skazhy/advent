(ns advent.2017.day8)


(def ^:private comp-ops {"!=" not= "<=" <= "==" = ">=" >= ">" > "<" <})
(def ^:private upd-ops {"inc" + "dec" -})

(defn- instruciton-fn [op arg] (fn [i] (op (or i 0) (Integer. arg))))

(defn- parse-instruction [row]
  ; format: [foo inc 10 if bar > 10]
  (when-let [[register upd-op upd-with _ compare-reg comp-op comp-with] row]
    {:register register
     :compare-fn (instruciton-fn (get comp-ops comp-op) comp-with)
     :update-fn (instruciton-fn (get upd-ops upd-op) upd-with)
     :compare-reg compare-reg}))

(defn- can-exec? [registers instruction]
  ((:compare-fn instruction) (get registers (:compare-reg instruction))))

(defn- apply-instructions [rows extract-result]
  (loop [rows rows registers {} max-value 0]
    (if-let [instruction (parse-instruction (first rows))]
      (if (can-exec? registers instruction)
        (let [res ((:update-fn instruction)
                    (get registers (:register instruction)))]
          (recur (rest rows)
                 (assoc registers (:register instruction) res)
                 (max max-value res)))
        (recur (rest rows) registers max-value))
      (extract-result registers max-value))))

(defn puzzle1 [rows]
  (apply-instructions rows (fn [registers _] (apply max (map last registers)))))

(defn puzzle2 [rows]
  (apply-instructions rows (fn [_ max-value] max-value)))
