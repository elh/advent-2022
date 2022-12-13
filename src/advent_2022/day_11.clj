(ns advent-2022.day-11
  (:require [clojure.string :as str]))

;; op must be an 2-arity infix function where each arg is either an integer or a symbol
(def op-fn (fn [op]
             (let [parts (str/split op #" ")
                   arg (fn [p]
                         (if (or (= (first p) \-) (Character/isDigit (first p)))
                           (Integer/parseInt p)
                           (symbol p)))
                   fn-list (list 'fn ['old]
                                 (list (arg (second parts))
                                       (arg (first parts))
                                       (arg (nth parts 2))))]
               (memoize (eval fn-list)))))

(defn parse-monkeys [file-name]
  (mapv (fn [m]
          (let [lines (str/split m #"\n")
                items (mapv #(Integer/parseInt %) (str/split (second (str/split (second lines) #"Starting items: ")) #", "))
                op-fn (op-fn (second (str/split (nth lines 2) #"Operation: new = ")))
                divisor (Integer/parseInt (second (str/split (nth lines 3) #"Test: divisible by ")))
                if-true (Integer/parseInt (second (str/split (nth lines 4) #"If true: throw to monkey ")))
                if-false (Integer/parseInt (second (str/split (nth lines 5) #"If false: throw to monkey ")))]
            {:items items, :op-fn op-fn, :divisor divisor, :if-true if-true, :if-false if-false, :inspected 0}))
        (str/split (slurp file-name) #"\n\n")))

(defn inspect-item [monkeys m-idx reduce-fn]
  (let [m (nth monkeys m-idx)
        item (first (:items m))
        new-worry (reduce-fn ((:op-fn m) item))
        test-result (zero? (mod new-worry (:divisor m)))
        move-idx (if test-result (:if-true m) (:if-false m))]
    (as-> monkeys ms
      (update-in ms [m-idx :items] (comp vec rest))
      (update-in ms [move-idx :items] conj new-worry)
      (update-in ms [m-idx :inspected] inc))))

(defn turn [monkeys m-idx reduce-fn]
  (loop [monkeys monkeys]
    (if (zero? (count (:items (nth monkeys m-idx))))
      monkeys
      (recur (inspect-item monkeys m-idx reduce-fn)))))

(defn round [monkeys reduce-fn]
  (reduce (fn [monkeys m-idx] (turn monkeys m-idx reduce-fn))
          monkeys
          (range (count monkeys))))

(defn rounds [monkeys n reduce-fn]
  (loop [monkeys monkeys n n]
    (if (zero? n)
      monkeys
      (recur (round monkeys reduce-fn) (dec n)))))

(defn relief [worry] (quot worry 3))

(defn reduce-lcm [monkeys] (fn [worry] (mod worry (reduce * (map #(:divisor %) monkeys)))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [monkeys (parse-monkeys (first args))]
    (time (println "part 1:" (reduce * (take-last 2 (sort (map #(:inspected %) (rounds monkeys 20 relief)))))))
    (time (println "part 2:" (reduce * (take-last 2 (sort (map #(:inspected %) (rounds monkeys 10000 (reduce-lcm monkeys))))))))))
