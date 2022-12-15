(ns advent-2022.day-14
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (mapv #(str/split % #" -> ") v)
    (mapv (fn [x] (mapv #(str/split % #",") x)) v)
    (mapv (fn [x] (mapv (fn [y] (mapv #(Integer/parseInt %) y)) x)) v)))

(defn draw-line [line]
  (loop [acc #{} l line]
    (if (<= (count l) 1)
      acc
      (let [xs (sort [(first (first l)) (first (second l))])
            ys (sort [(second (first l)) (second (second l))])
            locs (set (for [x (vec (range (first xs) (inc (second xs))))
                            y (vec (range (first ys) (inc (second ys))))]
                        [x y]))]
        (recur (set/union acc locs) (rest l))))))

(defn make-occupancy [input]
  (reduce (fn [acc line]
            (set/union acc (draw-line line)))
          #{} input))

(defn pour-sand [occupancy]
  (let [lowest (last (sort (map #(second %) occupancy)))]
    (loop [occupancy occupancy sand-loc [500 0]]
      (if (>= (second sand-loc) lowest)
        occupancy
        (cond
          (not (contains? occupancy (map + sand-loc [0 1]))) (recur occupancy (map + sand-loc [0 1]))
          (not (contains? occupancy (map + sand-loc [-1 1]))) (recur occupancy (map + sand-loc [-1 1]))
          (not (contains? occupancy (map + sand-loc [1 1]))) (recur occupancy (map + sand-loc [1 1]))
          :else (recur (conj occupancy sand-loc) [500 0]))))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))
        occupancy (make-occupancy input)]
    (println "part 1:" (- (count (pour-sand occupancy)) (count occupancy)))
    (println "part 2:" "TODO")))
