(ns advent-2022.day-23
  (:require [clojure.string :as str]))

(defn grid-to-coords [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (map-indexed #(filter some? (map-indexed (fn [idx itm]
                                               (when-not (= itm \.) [%1 idx])) %2)) v)
    (reduce into #{} v)))

(def starting-dir-order [[-1 0] [1 0] [0 -1] [0 1]])

(def all-surrounding-coords (filter #(not= [0 0] %) (for [y [-1 0 1]
                                                          x [-1 0 1]]
                                                      [y x])))
(defn has-neighbor [elf-coord coords]
  (true? (some true? (map (fn [delta]
                            (when (coords (map + elf-coord delta)) true))
                          all-surrounding-coords))))

;; return coordinates the elf would like to go to in this round
(defn propose-move [elf-coord coords dir-order]
  (if (or
       (not (has-neighbor elf-coord coords))
       (empty? dir-order))
    elf-coord
    (let [delta (first dir-order)
          empty-to-move-coords (if (zero? (first delta))
                                 (for [y [-1 0 1]] [y (second delta)])
                                 (for [x [-1 0 1]] [(first delta) x]))]
      (if (some #(coords (mapv + elf-coord %)) empty-to-move-coords)
        (propose-move elf-coord coords (rest dir-order))
        (mapv + elf-coord delta)))))

(defn round [coords dir-order]
  (let [proposed-coords (map (fn [x] {:cur x, :proposed (propose-move x coords dir-order)}) coords)
        proposed-counts (frequencies (map :proposed proposed-coords))]
    (reduce (fn [acc {:keys [cur proposed]}]
              (if (> (proposed-counts proposed) 1)
                (conj acc cur)
                (conj acc proposed)))
            #{} proposed-coords)))

(defn rounds [coords max-n]
  (loop [coords coords, n 0, dir-order starting-dir-order]
    (let [next-coords (round coords dir-order)]
      (if (or (>= n max-n) (= next-coords coords))
        {:coords coords :n (inc n)}
        (recur next-coords (inc n) (conj (vec (rest dir-order)) (first dir-order)))))))

(defn count-empty-tiles [coords]
  (let [min-x (apply min (map first coords))
        max-x (apply max (map first coords))
        min-y (apply min (map second coords))
        max-y (apply max (map second coords))]
    (- (* (inc (- max-x min-x)) (inc (- max-y min-y))) (count coords))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (grid-to-coords (first args))]
    (println "part 1:" (time (count-empty-tiles (:coords (rounds input 10)))))
    (println "part 2:" (time (:n (rounds input Integer/MAX_VALUE))))))
