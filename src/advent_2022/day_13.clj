(ns advent-2022.day-13
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n\n")
    (mapv #(edn/read-string (str "[" (str/replace % #"\n" ",") "]")) v)))

(defn right-order? [a b]
  (letfn [(ro? [a b]
            (if (number? a)
              (if (number? b)
                (case (compare a b)
                  -1 :right
                  0 :continue
                  1 :wrong)
                (ro? [a] b))
              (if (number? b)
                (ro? a [b])
                (if (empty? a)
                  (if (empty? b)
                    :continue
                    :right)
                  (if (empty? b)
                    :wrong
                    (case (ro? (first a) (first b))
                      :right :right
                      :continue (ro? (rest a) (rest b))
                      :wrong :wrong))))))]
    (= :right (ro? a b))))

(defn sum-right-indices [input]
  (->> input
       (mapv (fn [x] (right-order? (first x) (second x))))
       (map-indexed (fn [idx result] (if result (+ idx 1) 0)))
       (reduce +)))

;; part 2

(def decoder1 [[2]])
(def decoder2 [[6]])

(defn distress-signal [p2-input]
  (->> p2-input
       (sort right-order?)
       (map-indexed (fn [idx v] (if (or (= v decoder1) (= v decoder2)) (+ idx 1) 0)))
       (filter #(not= % 0))
       (reduce *)))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))
        p2-input (conj (reduce concat input) decoder1 decoder2)]
    (println "part 1:" (time (sum-right-indices input)))
    (println "part 2:" (time (distress-signal p2-input)))))
