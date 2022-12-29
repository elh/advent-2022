(ns advent-2022.day-17
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/trim-newline v)
    (str/split v #"")))

;; a rock is a seq of x, y coords
;; all rock coordinates are aligned with x=0 and y=0
(def rocks [[[0 0] [1 0] [2 0] [3 0]]       ;; minus
            [[0 1] [1 0] [1 1] [1 2] [2 1]] ;; plus
            [[0 0] [1 0] [2 0] [2 1] [2 2]] ;; reverse L
            [[0 0] [0 1] [0 2] [0 3]]       ;; bar
            [[0 0] [0 1] [1 0] [1 1]]])     ;; square

;; dir is "<" ">" or "" (fall)
(defn move-rock [rock dir]
  (let [delta (case dir
                "<" [-1 0]
                ">" [1 0]
                [0 -1])]
    (mapv #(mapv + % delta) rock)))

;; walls are at x=-1 and x=8, floor is at y=-1
(defn collides? [rock stopped]
  (boolean (or (some #(<= (first %) -1) rock)
               (some #(>= (first %) 7) rock)
               (some #(<= (second %) -1) rock)
               (some #(contains? stopped %) rock))))

(defn init-rock [rock-idx highest-y]
  (as-> (nth rocks rock-idx) rock
   (mapv #(mapv + % [2 0]) rock)
    (mapv #(mapv + % [0 (+ highest-y 4)]) rock)))

(defn drop-rock [rock stopped moves move-idx highest-y]
  (loop [rock rock move-idx move-idx]
    (let [cur-move (nth moves move-idx)
          next-rock (move-rock rock cur-move)
          next-move-idx (mod (inc move-idx) (count moves))]
      (if (collides? next-rock stopped)
        (if (= cur-move "")
          {:stopped (set/union stopped (set rock))
           :highest-y (max highest-y (reduce #(max %1 (second %2)) -1 rock))
           :next-move-idx next-move-idx}
          (recur rock next-move-idx))
        (recur next-rock next-move-idx)))))

(defn drop-rocks [moves max-rocks]
  (loop [rock-idx 0
         move-idx 0
         highest-y -1
         stopped #{}
         dropped 0]
    (if (>= dropped max-rocks)
      (inc highest-y)
      (let [rock (init-rock rock-idx highest-y)
            {next-move-idx :next-move-idx
             highest-y :highest-y
             stopped :stopped} (drop-rock rock stopped moves move-idx highest-y)
            next-rock-idx (mod (inc rock-idx) (count rocks))]
        (recur next-rock-idx next-move-idx highest-y  stopped (inc dropped))))))

(defn hydrate-moves [moves]
  (vec (flatten (map vector moves (repeat (count moves) "")))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (time (drop-rocks (hydrate-moves input) 2022)))
    (println "part 2:" "TODO")))
