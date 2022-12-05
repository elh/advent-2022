(ns advent-2022.day-4
  (:require [clojure.string :as str]))

(defn read-input
  [file-name]
  (vec (map str/trim-newline (str/split (slurp file-name) #"\n"))))

(defn full-overlap? [line]
  (let [parts (map #(Integer/parseInt %) (str/split line #",|-"))]
    (or
     (and (<= (first parts) (nth parts 2)) (>= (second parts) (nth parts 3)))
     (and (>= (first parts) (nth parts 2)) (<= (second parts) (nth parts 3))))))

; 2 ranges overlap if they they start before the other ends
(defn partial-overlap? [line]
  (let [parts (map #(Integer/parseInt %) (str/split line #",|-"))]
    (and (<= (first parts) (nth parts 3)) (>= (second parts) (nth parts 2)))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))

  (let [input (read-input (first args))]
    (println "part 1:" (->> input
                            (map full-overlap?)
                            (filter true?)
                            (count)))
    (println "part 2:" (->> input
                            (map partial-overlap?)
                            (filter true?)
                            (count)))))
