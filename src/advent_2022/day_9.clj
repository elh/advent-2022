(ns advent-2022.day-9
  (:require [clojure.string :as str]))

(defn read-input [file-name]
  (map str/trim-newline (str/split (slurp file-name) #"\n")))

(defn expanded-input [input]
  (flatten
   (map (fn [row]
          (let [parts (rest (re-find #"(\S+) (\d+)" row))]
            (repeat (Integer/parseInt (second parts)) (first parts)))) input)))

(defn move [steps n]
  (letfn [(move-head [pos step]
            (map + pos (case step "R" [0 1] "L" [0 -1] "D" [1 0] "U" [-1 0])))
          (move-knot [parents knot]
            (let [parent (last parents)
                  delta (map - parent knot)
                  new-knot (cond
                             (every? #(= 2 %) (map abs delta)) (mapv + (map #(/ % 2) delta) knot) ;; diagonal move
                             (= 2 (first delta)) [(inc (first knot)) (second parent)]             ;; weird snake moves
                             (= -2 (first delta)) [(dec (first knot)) (second parent)]
                             (= 2 (second delta)) [(first parent) (inc (second knot))]
                             (= -2 (second delta)) [(first parent) (dec (second knot))]
                             :else knot)]
              (conj parents new-knot)))
          (move-knots [acc step]
            (let [new-head (move-head (first (:knots acc)) step)
                  new-knots (reduce move-knot [new-head] (rest (:knots acc)))]
              {:knots new-knots :tail-seen (conj (:tail-seen acc) (last new-knots))}))]
    (reduce move-knots {:knots (repeat n [0 0]) :tail-seen #{}} steps)))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (count (:tail-seen (move (expanded-input input) 2))))
    (println "part 2:" (count (:tail-seen (move (expanded-input input) 10))))))
