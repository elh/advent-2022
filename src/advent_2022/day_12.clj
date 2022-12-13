(ns advent-2022.day-12
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.set :as set]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (mapv #(str/split % #"") v)))

(defn as-grid [input]
  (mapv (fn [x]
          (mapv (fn [y]
                  (if (and (not= y "S") (not= y "E"))
                    (- (int (first y)) 96)
                    y)) x)) input))

(defn print-mat [m spacer]
  (pp/cl-format *out* (str "~{~{~a~^" spacer "~}~%~}") m))

(defn get-y-x [grid value]
  (first (for [[y row] (map-indexed vector grid)
               [x val] (map-indexed vector row)
               :when (= val value)]
           [y x])))

;; assumes uniform distances between nodes.
(defn bfs-grid [grid start end-value]
  (loop [dist {start 0} queue [start]]
    (cond
      (empty? queue) nil
      (= (get-in grid (first queue)) end-value) (get dist (first queue))
      :else (let [cur (first queue)
                  queue (vec (rest queue))
                  cur-dist (get dist cur)
                  neighbors (->> (filter some? (for [[y x] [[-1 0] [1 0] [0 -1] [0 1]]
                                                     :let [y (+ y (first cur))
                                                           x (+ x (second cur))]]
                                                 (when (and (>= y 0) (< y (count grid))
                                                            (>= x 0) (< x (count (first grid))))
                                                   [y x])))
                                 (filter (fn [v] (letfn [(grid-value [v]
                                                           (case (get-in grid v)
                                                             "S" 1
                                                             "E" 26
                                                             (get-in grid v)))]
                                                   (or
                                                    (>= (grid-value cur) (grid-value v))
                                                    (<= (- (grid-value v) (grid-value cur)) 1))))))
                  new-dist (into {} (map vector neighbors (vec (repeat (count neighbors) (+ cur-dist 1)))))
                  new-nodes (seq (set/difference (set (keys new-dist)) (set (keys dist)) (set queue)))
                  dist (merge-with min dist new-dist)
                  queue (into queue new-nodes)]
              (recur dist queue)))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))
        grid (as-grid input)]
    (println "part 1:" (time (bfs-grid grid (get-y-x grid "S") "E")))
    (println "part 2:" "TODO")))
