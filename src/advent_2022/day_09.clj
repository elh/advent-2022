(ns advent-2022.day-09
  (:require [clojure.string :as str]))

(defn read-input [file-name]
  (map str/trim-newline (str/split (slurp file-name) #"\n")))

(defn expanded-input
  "Expand input sequence turning instructions like 'R 2, U 1' into 'R R U'."
  [input]
  (flatten
   (map (fn [row]
          (let [parts (rest (re-find #"(\S+) (\d+)" row))]
            (repeat (Integer/parseInt (second parts)) (first parts)))) input)))

(defn move
  "Move a rope with n knots through the steps. Returns final knot locs and a set of all locs the tail has seen."
  [steps n-knots]
  (letfn [(move-head [pos step]
            (map + pos (case step "R" [0 1] "L" [0 -1] "D" [1 0] "U" [-1 0])))
          (move-knot [parents knot]
            (let [parent (last parents)
                  delta (map - parent knot)
                  new-knot (cond
                             (every? #(= 2 %) (map abs delta)) (mapv + (map #(quot % 2) delta) knot) ;; diagonal move
                             (some #(= 2 %) (map abs delta)) (let [scootch (mapv #(quot % 2) delta)] ;; weird snake moves
                                                               (if (= 1 (abs (first scootch)))
                                                                 [(+ (first knot) (first scootch)) (second parent)]
                                                                 [(first parent) (+ (second knot) (second scootch))]))
                             :else knot)]
              (conj parents new-knot)))
          (move-knots [acc step]
            (let [new-head (move-head (first (:knots acc)) step)
                  new-knots (reduce move-knot [new-head] (rest (:knots acc)))]
              {:knots new-knots :tail-seen (conj (:tail-seen acc) (last new-knots))}))]
    (reduce move-knots {:knots (repeat n-knots [0 0]) :tail-seen #{}} steps)))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (count (:tail-seen (move (expanded-input input) 2))))
    (println "part 2:" (count (:tail-seen (move (expanded-input input) 10))))))
