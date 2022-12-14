(ns advent-2022.day-13
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn read-input [file-name]
  (mapv #(edn/read-string (str "[" (str/replace % #"\n" ",") "]"))
        (str/split (slurp file-name) #"\n\n")))

(defn right-order? [a b]
  (if (number? a)
    (if (number? b)
      (case (compare a b)
        -1 :right
        0 :continue
        1 :wrong)
      (right-order? [a] b))
    (if (number? b)
      (right-order? a [b])
      (if (empty? a)
        (if (empty? b)
          :continue
          :right)
        (if (empty? b)
          :wrong
          (case (right-order? (first a) (first b))
            :right :right
            :continue (right-order? (rest a) (rest b))
            :wrong :wrong))))))

(defn sum-right-indices [results]
  (reduce + (map-indexed (fn [idx result] (if (= result :right) (+ idx 1) 0)) results)))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (sum-right-indices (mapv (fn [x] (right-order? (first x) (second x))) input)))
    (println "part 2:" "TODO")))
