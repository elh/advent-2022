(ns advent-2022.day-8
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn read-input
  "Returns a 2D vector of integers."
  [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (mapv #(str/split % #"") v)
    (mapv (fn [x] (mapv #(Integer/parseInt %) x)) v)))

(defn print-mat [m]
  (pp/cl-format *out* "~{~{~a~^\t~}~%~}" m))

(defn visible-left [m]
  (mapv (fn [row]
          (:row (reduce (fn [acc x]
                    (if (< (:largest acc) x)
                      {:row (conj (:row acc) true) :largest x}
                      {:row (conj (:row acc) false) :largest (:largest acc)}))
                  {:row [] :largest -1}
                  row))) m))

(defn rotate-90 [m]
  (apply mapv vector (reverse m)))

(defn visible-rotation [m rotations]
  (assert (<= 0 rotations 3))
  (as-> m v
    (nth (iterate rotate-90 v) rotations)
    (visible-left v)
    (nth (iterate rotate-90 v) (- 4 rotations))))

(defn visible [m]
  (let [vis-ms (mapv (fn [r] (visible-rotation m r)) (range 4))]
    (reduce (fn [acc m]
              (vec (map-indexed (fn [i row]
                             (vec (map-indexed (fn [j v]
                                            (or v (get-in acc [i j]))) row))) m)))
            (first vis-ms)
            (rest vis-ms))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (reduce (fn [acc row]
                                 (+ acc (reduce (fn [acc v]
                                                  (if v (inc acc) acc)) 0 row)))
                               0
                               (visible input)))
    (println "part 2:" "TODO")))
