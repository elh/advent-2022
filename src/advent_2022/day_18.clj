(ns advent-2022.day-18
  (:require [clojure.string :as str]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (mapv #(str/split % #",") v)
    (mapv (fn [p] (mapv #(Integer/parseInt %) p)) v)))

(defn surface-area [points]
  (let [point-set (set points)]
    (reduce + (map (fn [x]
                     (reduce + (map (fn [delta]
                                      (if (point-set (map + x delta))
                                        0
                                        1))
                                    [[-1 0 0] [1 0 0] [0 -1 0] [0 1 0] [0 0 -1] [0 0 1]])))
                   points))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (time (surface-area input)))
    (println "part 2:" (time "TODO"))))
