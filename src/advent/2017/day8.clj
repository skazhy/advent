(ns advent.2017.day8
  "Advent of Code 2017, day 8: Register instruction parsing and execution")


(def ^:private comp-ops {"!=" not= "<=" <= "==" = ">=" >= ">" > "<" <})
(def ^:private upd-ops {"inc" + "dec" -})

(defn- instruction-fn [op arg] (fn [i] (op (or i 0) (Integer. arg))))

(defn- register-fn [register op arg]
  (fn [registers] ((instruction-fn op arg) (get registers register))))

(defn- parse-instruction [row]
  ; format: [foo inc 10 if bar > 10]
  (when-let [[register upd-op upd-with _ compare-reg comp-op comp-with] row]
    {:register register
     :compare-fn (register-fn compare-reg (get comp-ops comp-op) comp-with)
     :update-fn (register-fn register (get upd-ops upd-op) upd-with)}))

(defn- apply-instructions [rows extract-result]
  (loop [rows rows registers {} max-value 0]
    (if-let [instruction (parse-instruction (first rows))]
      (if ((:compare-fn instruction) registers)
        (let [res ((:update-fn instruction) registers)]
          (recur (rest rows)
                 (assoc registers (:register instruction) res)
                 (max max-value res)))
        (recur (rest rows) registers max-value))
      (extract-result registers max-value))))

(defn puzzle1 [rows]
  (apply-instructions rows (fn [registers _] (apply max (map last registers)))))

(defn puzzle2 [rows]
  (apply-instructions rows (fn [_ max-value] max-value)))
