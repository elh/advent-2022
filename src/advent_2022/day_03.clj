(ns advent-2022.day-03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input
  [file-name]
  (vec (map str/trim-newline (str/split (slurp file-name) #"\n"))))

(defn find-duplicate
  "Finds duplicate letter across first and second halves of the line."
  [line]
  (let [letters (str/split line #"")
        halves (split-at (quot (count letters) 2) letters)]
    (some (set (first halves)) (second halves))))

(defn find-duplicate-across-lines
  "Finds duplicate letter across 3 lines."
  [lines]
  (assert (= 3 (count lines)))
  (let [letters (map (fn [line] (str/split line #"")) lines)
        dupes (set/intersection (set (first letters)) (set (second letters)) (set (nth letters 2)))]
    (first dupes)))

(defn letter-priority [letter]
  (let [ascii-n (int (first letter))]
    (cond
      (<= 97 ascii-n 122) (- ascii-n 96)        ;; a-z 1-26
      (<= 65 ascii-n 90) (+ (- ascii-n 64) 26)  ;; A-Z 27-52
      :else (throw (Exception. "unexpected character")))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))

  (let [input (read-input (first args))]
    (println "part 1:" (time (->> input
                                  (map find-duplicate)
                                  (map letter-priority)
                                  (reduce +))))
    (println "part 2:" (time (->> input
                                  (partition 3)
                                  (map find-duplicate-across-lines)
                                  (map letter-priority)
                                  (reduce +))))))
