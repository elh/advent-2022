(ns advent-2022.day-22
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :as pp]))

(def verbose false)

(defn read-input [file-name]
  (letfn [(parse-board [s]
            (let [lines (str/split s #"\n")
                  max-x (count (apply max-key count lines))]
              (mapv (fn [l]
                      (vec (concat (reduce #(conj %1 (str %2)) [] l)
                                   (repeat (- max-x (count l)) " ")))) lines)))
          (parse-instrs [s]
            (loop [s s out []]
              (if (empty? s)
                out
                (let [maybe-number (re-find #"\d*" s)]
                  (if (empty? maybe-number)
                    (recur (str/replace s #"^\S" "") (conj out (re-find #"\S" s)))
                    (recur (str/replace s #"^\d*" "") (conj out (Integer/parseInt maybe-number))))))))]
    (as-> (slurp file-name) v
      (str/trim-newline v)
      (str/split v #"\n\n")
      (identity {:board (parse-board (first v))
                 :instrs (parse-instrs (second v))}))))

;; [y x]
(defn initial-pos [board]
  (loop [y 0 x 0]
    (when-not (>= y (count board))
      (if (>= x (count (first board)))
        (recur (inc y) 0)
        (if (= "." (get-in board [y x]))
          [y x]
          (recur y (inc x)))))))

;; first one is facing right. rotating to the clockwise (R)
;; rotating R will be +1 to the key, L is -1
(def dirs {0 [0 1] 1 [1 0] 2 [0 -1] 3 [-1 0]})
(def dirs-human {[0 1] "R" [1 0] "D" [0 -1] "L" [-1 0] "U"})
(def dirs-pw-values (set/map-invert dirs))

;; return final position after moving the given distance in the given direction on the board
(defn move [board pos dir dist]
  (let [cur-tile (get-in board pos)]
    (assert (not= cur-tile "#") (format "pos %s is wall" pos)))
  (letfn [(scootch [pos]
                   (let [next-pos (mapv + pos dir)]
                     [(mod (first next-pos) (count board))
                      (mod (second next-pos) (count (first board)))]))]
    (loop [pos pos next-pos (scootch pos) dist dist]
      (when verbose (println "pos:" pos "next-pos:" next-pos "dir:" (dirs-human dir) "dist:" dist))
      (if (zero? dist)
        pos
        (let [next-tile (get-in board next-pos)]
          (case next-tile
            "#" pos
            "." (recur next-pos (scootch next-pos) (dec dist))
            " " (recur pos (scootch next-pos) dist)
            (throw (Exception. (format "unexpected tile type at pos %s" pos)))))))))

;; pos as an [y x] :loc and [y x] delta :direction
(defn run [board instrs]
  (loop [instrs instrs pos (initial-pos board) dir-idx 0]
    (when (nil? pos)
      (throw (Exception. "could not find initial position")))
    (if (empty? instrs)
      {:pos pos :dir (dirs dir-idx)}
      (if (integer? (first instrs))
        (do
          (when verbose (println "\nMOVE" (first instrs) (dirs-human (dirs dir-idx))))
          (let [next-pos (move board pos (dirs dir-idx) (first instrs))]
            (recur (rest instrs) next-pos dir-idx)))
        (do
          (when verbose (println "\nROTATE" (first instrs)))
          (let [next-dir (if (= (first instrs) "R") inc dec)]
            (recur (rest instrs) pos (mod (next-dir dir-idx) (count dirs)))))))))

(defn final-password [{pos :pos dir :dir}]
  (+ (* 1000 (inc (first pos)))
     (* 4 (inc (second pos)))
     (dirs-pw-values dir)))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (time (final-password (run (:board input) (:instrs input)))))
    (println "part 2:" (time "TODO"))))
