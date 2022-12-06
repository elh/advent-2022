(ns advent-2022.day-6
  (:require [clojure.string :as str]))

(defn first-n-unique
  "Return number of chars read before we see first sliding window of n unique chars."
  [n line]
  (loop [i 0]
    (if (= n (count (distinct (subs line i (+ i n)))))
      (+ i n)
      (recur (inc i)))))

(defn -main [& args]
  (when (not= (count args) 1) (throw (Exception. "FAIL: expects input file as cmdline arg.")))
  (let [input (str/trim-newline (slurp (first args)))]
    (println "part 1:" (first-n-unique 4 input))
    (println "part 2:" (first-n-unique 14 input))))
