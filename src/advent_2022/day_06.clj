(ns advent-2022.day-06
  (:require [clojure.string :as str]))

(defn first-n-unique
  "Returns number of chars read before we see first sliding window of n unique chars."
  [n line]
  (loop [i 0]
    (if (= n (count (distinct (subs line i (+ i n)))))
      (+ i n)
      (recur (inc i)))))

;; ;; naive recusive solution stack overflows. must use loop/recur or for recursion.
;; (defn first-n-unique-recursion
;;   "Returns number of chars read before we see first sliding window of n unique chars."
;;   ([n line] (first-n-unique-recursion n line 0))
;;   ([n line cur]
;;    (if (= n (count (distinct (subs line cur (+ cur n)))))
;;      (+ cur n)
;;      (first-n-unique-recursion n line (inc cur)))))

(defn -main [& args]
  (when (not= (count args) 1) (throw (Exception. "FAIL: expects input file as cmdline arg.")))
  (let [input (str/trim-newline (slurp (first args)))]
    (println "part 1:" (time (first-n-unique 4 input)))
    (println "part 2:" (time (first-n-unique 14 input)))))
