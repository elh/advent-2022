(ns advent-2022.day-11
  (:require [clojure.string :as str]))

;; note: i couldn't figure out the clojure way of creating a function at runtime that takes arguments.
;; the issue is that evaluated forms are not in the lexical scope of the eval.
(defn eval-op [op old]
  (let [parts (str/split op #" ")
        body (list
              (symbol (second parts))
              (if (= "old" (first parts))
                old
                (Integer/parseInt (first parts)))
              (if (= "old" (nth parts 2))
                old
                (Integer/parseInt (nth parts 2))))]
    (eval body)))

(defn parse-monkeys [file-name]
  (mapv (fn [m]
         (let [lines (str/split m #"\n")
               items (mapv #(Integer/parseInt %) (str/split (second (str/split (second lines) #"Starting items: ")) #", "))
               op (second (str/split (nth lines 2) #"Operation: new = "))
               divisor (Integer/parseInt (second (str/split (nth lines 3) #"Test: divisible by ")))
               if-true (Integer/parseInt (second (str/split (nth lines 4) #"If true: throw to monkey ")))
               if-false (Integer/parseInt (second (str/split (nth lines 5) #"If false: throw to monkey ")))]
           {:items items, :op op, :divisor divisor, :if-true if-true, :if-false if-false, :inspected 0}))
       (str/split (slurp file-name) #"\n\n")))

;; always inspects first item
(defn inspect-item [monkeys m-idx]
  (let [m (nth monkeys m-idx)
        item (first (:items m))
        new-worry (quot (eval-op (:op m) item) 3)
        test-result (zero? (mod new-worry (:divisor m)))
        move-idx (if test-result (:if-true m) (:if-false m))] 
    (as-> monkeys ms
      (update-in ms [m-idx :items] (comp vec rest))
      (update-in ms [move-idx :items] conj new-worry)
      (update-in ms [m-idx :inspected] inc))))

(defn turn [monkeys m-idx]
  (loop [monkeys monkeys]
    (if (zero? (count (:items (nth monkeys m-idx))))
      monkeys
      (recur (inspect-item monkeys m-idx)))))

(defn round [monkeys]
  (reduce (fn [monkeys m-idx] (turn monkeys m-idx))
          monkeys
          (range (count monkeys))))

(defn rounds [monkeys n]
  (loop [monkeys monkeys n n]
    (if (zero? n)
      monkeys
      (recur (round monkeys) (dec n)))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [monkeys (parse-monkeys (first args))]
    (println "part 1:" (reduce * (take-last 2 (sort (map #(:inspected %) (rounds monkeys 20))))))
    (println "part 2:" "TODO")))
