(ns cs373.unit1.core)

(defn uniform-dist
  "Creates a 1-D world of size [size] with same probability for all cells. See video 6. Uniform Distribution"
  [size]
  (take size
    (repeat (/ 1 size))))

(defn reweight
  "Return a function that calculates the probability after sense. See video 10. Phit and Pmiss"
  [observ hit miss]
  (fn [prob value]
    (* prob
       (cond
         (= observ value) hit
         :else            miss))))

(defn normalize
  "Normalize probabilities. See videos 9. Normalize Distribution and 11. Sum of Probabilities"
  [coll]
  (let [sum (reduce + coll)]
    (map #(/ % sum) coll)
    ))

(defn sense
  "Sense function. See videos 13. Normalized Sense Function, 14. Test Sense Function and 15. Multiple Measurements"
  [probs world observ hit miss]
  (let [f              (reweight observ hit miss)
        non-normalized (map f probs world)]
    (normalize non-normalized)))

(defn prob-world
  "Helper function for Inexact move function"
  [prob world]
  (map #(* prob %) world))

(defn move
  "Exact move function. See video 17. Move Function"
  ([world steps]
    (let [ world-size         (count world)
           dest               (mod (* -1 steps) world-size)
           [passed remaining] (split-at dest world)]
      (concat remaining passed)))
  "Inexact move function. See video 21. Inexact Move Function"
  ([world steps & probs]
    (apply map +
      (map prob-world
        probs
        (for [step (range (dec steps) (+ 2 steps))]
          (move world step))))))

(defn where-am-i?
  "Sense and Move, combined. See videos 25. Sense and Move and 26. Sense and Move 2"
  [probs world unsensed hit miss motion undershoot exact overshoot]
  (loop [prob-state probs
         measures   unsensed
         movements  motion]
    (let [cell (first measures) step (first movements)]
      (if (and cell step)
        (recur
          (move (sense prob-state world cell hit miss) step undershoot exact overshoot)
          (rest measures)
          (rest movements))
        prob-state))))

(comment
  "Run code below to see the same result as in question 26. Sense and Move 2")

(def p (uniform-dist 5))
(def world [:green :red :red :green :green])
(def measurements [:red :red])
(def motion [1 1])

(def hit 0.6)
(def miss 0.2)

(def undershoot 0.1)
(def exact 0.8)
(def overshoot 0.1)

(where-am-i? p world measurements hit miss motion undershoot exact overshoot)