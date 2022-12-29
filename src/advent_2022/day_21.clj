(ns advent-2022.day-21
  (:require [clojure.string :as str]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (map (fn [l] (rest (re-find #"(\S+): (.*)" l))) v)
    (reduce (fn [acc x]
              (if (str/includes? (second x) " ")
                (let [parts (rest (re-find #"(\S+) (\S+) (\S+)" (second x)))]
                     (assoc acc (first x) {:op (second parts) :args [(first parts) (nth parts 2)]}))
                (assoc acc (first x) {:value (Integer/parseInt (second x))})))
            {} v)))

(defn get-value [input k]
  (let [v (input k)] 
    (or
     (:value v)
     ((eval (read-string (:op v))) (get-value input (get-in v [:args 0])) (get-value input (get-in v [:args 1]))))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (time (get-value input "root")))
    (println "part 2:" (time "TODO"))))
