(ns advent.2017.day8)


(def comp-ops
  {"!=" not= "<=" <= "==" = ">=" >= ">" > "<" <})

(def upd-ops
  {"inc" + "dec" -})

(defn parse-instruction [row]
  (let [[dest-reg upd-op upd-with _ src-reg comp-op comp-with] row]
    {:dest-reg dest-reg
     :compare #((get comp-ops comp-op) (or % 0) (Integer. comp-with))
     :update #((get upd-ops upd-op) (or % 0) (Integer. upd-with))
     :src-reg src-reg}))

(defn can-exec? [registers instruction]
  ((:compare instruction) (get registers (:src-reg instruction))))

(defn updated-register
  "Performs the `update` operation on the destination register."
  [registers instruction]
  ((:update instruction) (get registers (:dest-reg instruction))))

(defn apply-instructions [rows extract-result]
  (loop [rows rows registers {} max-value 0]
    (if (seq rows)
      (let [instruction (parse-instruction (first rows))]
        (if (can-exec? registers instruction)
          (let [res (updated-register registers instruction)]
            (recur (rest rows)
                   (assoc registers (:dest-reg instruction) res)
                   (max max-value res)))
          (recur (rest rows) registers max-value)))
      (extract-result registers max-value))))

(defn puzzle1 [rows]
  (apply-instructions rows (fn [regs _] (apply max (map last regs)))))

(defn puzzle2 [rows]
  (apply-instructions rows (fn [_ max-value] max-value)))
