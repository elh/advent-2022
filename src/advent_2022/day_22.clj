(ns advent-2022.day-22
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

;; first one is facing right
;; rotating R will be +1 to the key, L is -1
(def dirs {0 [0 1] 1 [1 0] 2 [0 -1] 3 [-1 0]})
(def dirs-pw-values (set/map-invert dirs))
(def dirs-human {[0 1] "R" [1 0] "D" [0 -1] "L" [-1 0] "U"})
(def dirs-human-invert (set/map-invert dirs-human))

;; part 1 move-fn
(defn move-flat [board pos dir]
  (let [next-pos (mapv + pos dir)]
    {:dir dir
     :pos [(mod (first next-pos) (count board))
           (mod (second next-pos) (count (first board)))]}))

(defn square-length [board]
  (as-> (reduce (fn [acc l]
                  (reduce #(if (not= %2 " ") (inc %1) %1) acc l))
                0 board) v
    (/ v 6)
    (int (Math/sqrt v))))

;; from is a boolean. true -> from edge, false -> to edge
(defn edge [board hexamino-edge from]
  (let [square-len (square-length board)
        corner (mapv * [square-len square-len] (:square hexamino-edge))]
    (reduce (fn [acc v]
              (conj acc [(case (:dir hexamino-edge)
                           "U" (if from
                                 [(dec (first corner)) (+ (second corner) v)]
                                 [(first corner) (+ (second corner) v)])
                           "D" (if from
                                 [(+ (first corner) square-len) (+ (second corner) v)]
                                 [(+ (first corner) (dec square-len)) (+ (second corner) v)])
                           "L" (if from
                                 [(+ (first corner) v) (dec (second corner))]
                                 [(+ (first corner) v) (second corner)])
                           "R" (if from
                                 [(+ (first corner) v) (+ (second corner) square-len)]
                                 [(+ (first corner) v) (+ (second corner) (dec square-len))]))
                         (:dir hexamino-edge)])) [] (range square-len))))

(defn make-cube [board hexamino-grid]
  (reduce (fn [acc hexamino-kv]
            ;; U -> U, R reversed, L -> L, D reversed, D -> D, L reversed, R -> R, U reversed
            (let [from-edge (edge board (first hexamino-kv) true)
                  to-edge (let [e (edge board (second hexamino-kv) false)]
                            (if (or (= (:dir (first hexamino-kv)) (:dir (second hexamino-kv)))
                                    (and (= (:dir (first hexamino-kv)) "U") (= (:dir (second hexamino-kv)) "R"))
                                    (and (= (:dir (first hexamino-kv)) "L") (= (:dir (second hexamino-kv)) "D"))
                                    (and (= (:dir (first hexamino-kv)) "D") (= (:dir (second hexamino-kv)) "L"))
                                    (and (= (:dir (first hexamino-kv)) "R") (= (:dir (second hexamino-kv)) "U")))
                              (vec (rseq e))
                              e))
                  edge-m (zipmap from-edge to-edge)]
              (merge acc edge-m)))
          {} hexamino-grid))

;; part 2 move-fn
(defn move-cube [cube pos dir]
  (let [next-pos (map + pos dir)
        dir-str (dirs-human dir)]
    (if (contains? cube [next-pos dir-str])
      (let [next (get-in cube [[next-pos dir-str]])]
        {:dir (mapv * [-1 -1] (dirs-human-invert (second next)))
         :pos (first next)})
      {:dir dir
       :pos next-pos})))

;; return final position and dir after moving the given distance in the given direction on the board
(defn move [board pos dir dist move-fn]
  (let [cur-tile (get-in board pos)]
    (assert (not= cur-tile "#") (format "pos %s is wall" pos)))
  (loop [out pos pos pos dir dir dist dist]
    (let [next (move-fn pos dir)]
      (when verbose (println "pos:" pos "next:" next "dist:" dist))
      (if (zero? dist)
        {:pos pos :dir dir}
        (let [next-tile (get-in board (:pos next))]
          (case next-tile
            "#" {:pos out :dir dir}
            "." (recur (:pos next) (:pos next) (:dir next) (dec dist))
            " " (recur out (:pos next) (:dir next) dist)
            (throw (Exception. (format "unexpected tile type at pos %s" (:pos next))))))))))

;; pos as an [y x] :loc and [y x] delta :direction
(defn run [board instrs move-fn]
  (loop [instrs instrs pos (initial-pos board) dir-idx 0]
    (when (nil? pos)
      (throw (Exception. "could not find initial position")))
    (if (empty? instrs)
      {:pos pos :dir (dirs dir-idx)}
      (if (integer? (first instrs))
        (do
          (when verbose (println "\nMOVE" (first instrs) (dirs-human (dirs dir-idx))))
          (let [next (move board pos (dirs dir-idx) (first instrs) move-fn)]
            (recur (rest instrs) (:pos next) (dirs-pw-values (:dir next)))))
        (do
          (when verbose (println "\nROTATE" (first instrs)))
          (let [next-dir (if (= (first instrs) "R") inc dec)]
            (recur (rest instrs) pos (mod (next-dir dir-idx) (count dirs)))))))))

(defn final-password [{pos :pos dir :dir}]
  (+ (* 1000 (inc (first pos)))
     (* 4 (inc (second pos)))
     (dirs-pw-values dir)))

;; cube folding
;;
;; manually specifying the configuration of 2 of the 11 possible cube-folding hexaminos... this does not handle
;; reflection and rotation but it could. generalize by supporting all unique hexaminos and all equivalent translations

(def example-hexamino-grid
  {{:square [0 2] :dir "U"} {:square [1 0] :dir "U"}
   {:square [0 2] :dir "L"} {:square [1 1] :dir "U"}
   {:square [0 2] :dir "R"} {:square [2 3] :dir "R"}
   {:square [1 0] :dir "U"} {:square [0 2] :dir "U"}
   {:square [1 0] :dir "L"} {:square [2 3] :dir "D"}
   {:square [1 0] :dir "D"} {:square [2 2] :dir "D"}
   {:square [1 1] :dir "U"} {:square [0 2] :dir "L"}
   {:square [1 1] :dir "D"} {:square [2 2] :dir "L"}
   {:square [1 2] :dir "R"} {:square [2 3] :dir "U"}
   {:square [2 2] :dir "L"} {:square [1 1] :dir "D"}
   {:square [2 2] :dir "D"} {:square [1 0] :dir "D"}
   {:square [2 3] :dir "U"} {:square [1 2] :dir "R"}
   {:square [2 3] :dir "R"} {:square [0 2] :dir "R"}
   {:square [2 3] :dir "D"} {:square [1 0] :dir "L"}})

(def input-hexamino-grid
  {{:square [0 1] :dir "U"} {:square [3 0] :dir "L"}
   {:square [0 1] :dir "L"} {:square [2 0] :dir "L"}
   {:square [0 2] :dir "U"} {:square [3 0] :dir "D"}
   {:square [0 2] :dir "R"} {:square [2 1] :dir "R"}
   {:square [0 2] :dir "D"} {:square [1 1] :dir "R"}
   {:square [1 1] :dir "L"} {:square [2 0] :dir "U"}
   {:square [1 1] :dir "R"} {:square [0 2] :dir "D"}
   {:square [2 1] :dir "R"} {:square [0 2] :dir "R"}
   {:square [2 1] :dir "D"} {:square [3 0] :dir "R"}
   {:square [2 0] :dir "U"} {:square [1 1] :dir "L"}
   {:square [2 0] :dir "L"} {:square [0 1] :dir "L"}
   {:square [3 0] :dir "L"} {:square [0 1] :dir "U"}
   {:square [3 0] :dir "R"} {:square [2 1] :dir "D"}
   {:square [3 0] :dir "D"} {:square [0 2] :dir "U"}})

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (time (final-password (run (:board input) (:instrs input) (partial move-flat (:board input))))))
    (let [hexamino-grid (if (str/includes? (first args) "example")
                          example-hexamino-grid
                          input-hexamino-grid)]
      (println "part 2:" (time (final-password (run (:board input) (:instrs input) (partial move-cube (make-cube (:board input) hexamino-grid)))))))))
