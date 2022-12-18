(ns advent-2022.day-15
  (:require [clojure.string :as str]))

(defn manhattan [pair]
  (+ (Math/abs (- (:sensor-x pair) (:beacon-x pair)))
     (Math/abs (- (:sensor-y pair) (:beacon-y pair)))))

;; we'll call this output a "pair"
(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (mapv (fn [l] (let [parts (rest (re-find #"Sensor at x=(\S+), y=(\S+): closest beacon is at x=(\S+), y=(\S+)" l))]
                    {:sensor-x (Integer/parseInt (first parts))
                     :sensor-y (Integer/parseInt (second parts))
                     :beacon-x (Integer/parseInt (nth parts 2))
                     :beacon-y (Integer/parseInt (nth parts 3))})) v)
    (mapv #(assoc % :manhattan (manhattan %)) v)))

;; if overlaps, returns a range as a tuple of x_start, x_end on the y row
;; else, returns nil
(defn coverage [pair y]
  (let [y-diff (Math/abs (- (:sensor-y pair) y))]
    (if (< (:manhattan pair) y-diff)
      nil
      (let [x_diff (- (:manhattan pair) y-diff)
            x_start (- (:sensor-x pair) x_diff)
            x_end (+ (:sensor-x pair) x_diff)]
        [x_start x_end]))))

;; represent as as non-overlapping ranges. also includes beacon and sensor positions
(defn coverage-all [pairs y]
  (let [all-ranges (reduce (fn [acc p]
                             (let [c (coverage p y)]
                               (if c
                                 (conj acc c)
                                 acc)))
                           []
                           pairs)]
    (reduce (fn [acc r]
              (if (empty? acc)
                [r]
                (let [last (last acc)]
                  (if (<= (first r) (second last))
                    (conj (vec (butlast acc)) [(first last) (max (second r) (second last))])
                    (conj acc r)))))
            []
            (sort-by first (into [] (concat all-ranges))))))

(defn count-free-positions [pairs y]
  (let [ranges (coverage-all pairs y)
        beacon-pos-on-y (set (map :beacon-x (filter #(= y (:beacon-y %)) pairs)))
        range-sum (reduce (fn [acc r]
                            (+ acc (inc (- (second r) (first r)))))
                          0
                          ranges)]
    (- range-sum (count beacon-pos-on-y))))

(defn -main [& args]
  (when (not= (count args) 2)
    (throw (Exception. (format "FAIL: expects input file and `y` as cmdline args. got %d args" (count args)))))
  (let [input (read-input (first args))
        n (Integer/parseInt (second args))]
    (println "part 1:" (time (count-free-positions input n)))
    (println "part 2:" (time "TODO"))))
