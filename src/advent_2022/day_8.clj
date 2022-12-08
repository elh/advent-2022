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

;; I first wrote this hard coding in a specific way of iterating through the matrix.
;; When I sought to generalize, it might be simpler to rotate the entire matrix than to
;; make iteration general.
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

(defn visible-from-rotation [m rotations]
  (assert (<= 0 rotations 3))
  (as-> m v
    (nth (iterate rotate-90 v) rotations)
    (visible-left v)
    (nth (iterate rotate-90 v) (- 4 rotations))))

;; Leverage our straightforward visible-left fn by just rotating the entire matrix to process
;; it from all 4 direction!
(defn visible
  "Returns a 2D vector of bools, where true means visible."
  [m]
  (let [vis-ms (mapv (fn [r] (visible-from-rotation m r)) (range 4))]
    (reduce (fn [acc m]
              (vec (map-indexed (fn [i row]
                                  (vec (map-indexed (fn [j v]
                                                      (or v (get-in acc [i j]))) row))) m)))
            (first vis-ms)
            (rest vis-ms))))

(defn count-visible [m]
  (reduce (fn [acc row]
            (+ acc (reduce (fn [acc v]
                             (if v (inc acc) acc)) 0 row)))
          0
          (visible m)))

;; Too lazy to make this general... This complex solution actually bit me because I had a bug in
;; how I was handling the current row/col. I was using the current row/col to compare heights
;; instead of the original height.
(defn scenic-score [m y x]
  (assert (= (count m) (count (first m)))) ;; require square matrix
  (let [start-height (get-in m [y x])
        right (loop [acc 0 cur-x x]
                (cond
                  (>= cur-x (dec (count m))) acc
                  (<= start-height (get-in m [y (inc cur-x)])) (inc acc)
                  :else (recur (inc acc) (inc cur-x))))
        left (loop [acc 0 cur-x x]
               (cond
                 (<= cur-x 0) acc
                 (<= start-height (get-in m [y (dec cur-x)])) (inc acc)
                 :else (recur (inc acc) (dec cur-x))))
        down (loop [acc 0 cur-y y]
               (cond
                 (>= cur-y (dec (count m))) acc
                 (<= start-height (get-in m [(inc cur-y) x])) (inc acc)
                 :else (recur (inc acc) (inc cur-y))))
        up (loop [acc 0 cur-y y]
             (cond
               (<= cur-y 0) acc
               (<= start-height (get-in m [(dec cur-y) x])) (inc acc)
               :else (recur (inc acc) (dec cur-y))))]
    (* right left down up)))

(defn max-scenic-score [m]
  ;; replace double loop with "for"?
  (loop [max-score 0 y 0]
    (if (>= y (count m))
      max-score
      (recur (max max-score (loop [max-score-row 0 x 0]
                              (if (>= x (count m))
                                max-score-row
                                (recur (max max-score-row (scenic-score m y x))
                                       (inc x)))))
             (inc y)))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (count-visible input))
    (println "part 2:" (max-scenic-score input))))
