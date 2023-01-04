(ns advent-2022.day-18
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (mapv #(str/split % #",") v)
    (mapv (fn [p] (mapv #(Integer/parseInt %) p)) v)))

;; no diagonals
(defn neighbors [p] (map #(map + p %)
                         [[-1 0 0] [1 0 0] [0 -1 0] [0 1 0] [0 0 -1] [0 0 1]]))

;; for every lava point, count the number of adjacent points that are air
(defn surface-area [points]
  (let [point-set (set points)]
    (reduce + (map (fn [x]
                     (reduce + (map (fn [neighbor]
                                      (if (point-set neighbor) 0 1))
                                    (neighbors x))))
                   points))))

;; with 1 space of padding
(defn bounding-cube [points]
  (letfn [(value [min-max idx]
                 (let [v (apply min-max (map #(nth % idx) points))]
                   (if (= min-max min)
                     (dec v)
                     (inc v))))]
    {:min-x (value min 0) :max-x (value max 0)
     :min-y (value min 1) :max-y (value max 1)
     :min-z (value min 2) :max-z (value max 2)}))

(defn in-bounding-cube [p cube]
  (and (<= (:min-x cube) (first p) (:max-x cube))
       (<= (:min-y cube) (second p) (:max-y cube))
       (<= (:min-z cube) (nth p 2) (:max-z cube))))

(defn fill-cube
  "Returns a set of all points on the outside of the surface inside the bounding cube"
  ([point-set bounding-cube] (let [start [(:min-x bounding-cube) (:min-y bounding-cube) (:min-z bounding-cube)]]
                               (fill-cube point-set bounding-cube [start] #{start})))
  ([point-set bounding-cube q seen] ;; when adding to the queue, immediately add it to seen to de-dup
   (if (empty? q)
     seen
     (let [cur (first q)
           candidates (reduce (fn [acc p]
                                (if (and (in-bounding-cube p bounding-cube) (not (seen p)) (not (point-set p)))
                                  (conj acc p)
                                  acc))
                              []
                              (neighbors cur))]
       (recur point-set bounding-cube (vec (concat (rest q) candidates)) (set/union seen (set candidates)))))))

;; find all exterior air within a bounding box and count all surfaces it touches the lava
(defn exterior-surface-area
  [points]
  (let [point-set (set points)
        bounding-cube (bounding-cube points)
        exterior-air (fill-cube point-set bounding-cube)]
    (reduce + (map (fn [x]
                     (reduce + (map (fn [neighbor]
                                      (if (point-set neighbor) 1 0))
                                    (neighbors x))))
                   exterior-air))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (time (surface-area input)))
    (println "part 2:" (time (exterior-surface-area input)))))
