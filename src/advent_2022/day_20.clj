(ns advent-2022.day-20
  (:require [clojure.string :as str]))

(defn with-ids [data]
  (vec (map-indexed (fn [i v] {:id i :v v}) data)))

;; all functions expect "data with ids"
(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (mapv #(Integer/parseInt %) v)
    (with-ids v)))

(defn mix-1 [data idx]
  (let [element (get-in data [idx])
        v (get-in data [idx :v])
        new-idx (if (zero? (+ idx v))
                  (dec (count data))
                  (mod (+ idx v) (dec (count data))))]
    (cond
      (= idx new-idx) data
      (< idx new-idx) (into [] (concat (subvec data 0 idx) (subvec data (inc idx) (inc new-idx)) [element] (subvec data (inc new-idx))))
      (> idx new-idx) (into [] (concat (subvec data 0 new-idx) [element] (subvec data new-idx idx) (subvec data (inc idx)))))))

(defn mix [data]
  (let [ids (map :id data)]
    (reduce (fn [acc id]
              (let [element (first (filter #(= (:id (second %)) id) (map-indexed vector acc)))]
                (mix-1 acc (first element))))
            data
            ids)))

(defn grove-coordinates [data]
  (let [zero-element (first (filter #(= (:v (second %)) 0) (map-indexed vector data)))]
    (letfn [(nth-value-after-zero [n]
                                  (:v (nth data (mod (+ n (first zero-element)) (count data)))))]
      (reduce (fn [acc v] (+ acc (nth-value-after-zero v))) 0 [1000 2000 3000]))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (time (grove-coordinates (mix input))))
    (println "part 2:" (time "TODO"))))
