(ns advent-2022.day-01
  (:require [clojure.string :as str]))

(defn read-input
  [file-name]
  (vec (map str/trim-newline (str/split (slurp file-name) #"\n"))))

(defn elves [calorie-list]
  (letfn [(agg [vs v]
            (if (str/blank? v)
              (conj vs 0)
              (conj (vec (butlast vs)) (+ (last vs) (Integer/parseInt v)))))]
    (reduce agg [0] calorie-list)))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))

  (let [input (read-input (first args))]
    (println "part 1:" (apply max (elves input)))
    (println "part 2:" (reduce + (take-last 3 (sort (elves input)))))))
