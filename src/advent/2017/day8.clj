(ns advent.2017.day8)


(def ^:private comp-ops {"!=" not= "<=" <= "==" = ">=" >= ">" > "<" <})
(def ^:private upd-ops {"inc" + "dec" -})

(defn- instruciton-fn [op arg] (fn [i] (op (or i 0) (Integer. arg))))

(defn- parse-instruction [row]
  ; format: [foo inc 10 if bar > 10]
  (when-let [[dest-reg upd-op upd-with _ src-reg comp-op comp-with] row]
    {:dest-reg dest-reg
     :compare (instruciton-fn (get comp-ops comp-op) comp-with)
     :update (instruciton-fn (get upd-ops upd-op) upd-with)
     :src-reg src-reg}))

(defn- can-exec? [registers instruction]
  ((:compare instruction) (get registers (:src-reg instruction))))

(defn- apply-instructions [rows extract-result]
  (loop [rows rows registers {} max-value 0]
    (if-let [instruction (parse-instruction (first rows))]
      (if (can-exec? registers instruction)
        (let [res ((:update instruction)
                    (get registers (:dest-reg instruction)))]
          (recur (rest rows)
                 (assoc registers (:dest-reg instruction) res)
                 (max max-value res)))
        (recur (rest rows) registers max-value))
      (extract-result registers max-value))))

(defn puzzle1 [rows]
  (apply-instructions rows (fn [regs _] (apply max (map last regs)))))

(defn puzzle2 [rows]
  (apply-instructions rows (fn [_ max-value] max-value)))
