(ns advent-2022.day-17
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/trim-newline v)
    (str/split v #"")))

(defn hydrate-moves [moves]
  (vec (flatten (map vector moves (repeat (count moves) "")))))

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
(defn collides? [rock occupancy]
  (boolean (or (some #(<= (first %) -1) rock)
               (some #(>= (first %) 7) rock)
               (some #(<= (second %) -1) rock)
               (some #(contains? occupancy %) rock))))

(defn init-rock [rock-idx highest-y]
  (as-> (nth rocks rock-idx) rock
   (mapv #(mapv + % [2 0]) rock)
    (mapv #(mapv + % [0 (+ highest-y 4)]) rock)))

(defn drop-rock [rock occupancy moves move-idx highest-y]
  (loop [rock rock move-idx move-idx]
    (let [cur-move (nth moves move-idx)
          next-rock (move-rock rock cur-move)
          next-move-idx (mod (inc move-idx) (count moves))]
      (if (collides? next-rock occupancy)
        (if (= cur-move "")
          {:occupancy (set/union occupancy (set rock))
           :stopped-at (first rock)
           :highest-y (max highest-y (reduce #(max %1 (second %2)) -1 rock))
           :next-move-idx next-move-idx}
          (recur rock next-move-idx))
        (recur next-rock next-move-idx)))))

(defn drop-rocks [moves max-rocks]
  (loop [rock-idx 0
         move-idx 0
         highest-y -1
         occupancy #{}
         ;; rock type -> seq where they stopped at. represented as the first coord in the rock vector of coords
         stopped-rocks (reduce #(assoc %1 %2 []) {} (range (count rocks)))
         ;; rock type -> tower height after block is dropped
         after-drop-heights (reduce #(assoc %1 %2 []) {} (range (count rocks)))
         dropped 0]
    (if (>= dropped max-rocks)
      {:height (inc highest-y)
       :stopped-rocks stopped-rocks
       :after-drop-heights after-drop-heights}
      (let [rock (init-rock rock-idx highest-y)
            {next-move-idx :next-move-idx
             highest-y :highest-y
             occupancy :occupancy
             stopped-at :stopped-at} (drop-rock rock occupancy moves move-idx highest-y)
            next-rock-idx (mod (inc rock-idx) (count rocks))]
        (recur
         next-rock-idx next-move-idx highest-y occupancy
         (update-in stopped-rocks [rock-idx] conj stopped-at)
         (update-in after-drop-heights [rock-idx] conj highest-y)
         (inc dropped))))))

(defn check-cyle-point [rocks start-idx cycle-size]
  (let [coord-delta (mapv - (nth rocks (+ start-idx cycle-size)) (nth rocks start-idx))]
    (loop [last-idx (+ start-idx cycle-size)
           check-idx (+ start-idx (* 2 cycle-size))
           first-iteration true]
      (if (>= check-idx (count rocks))
        (not first-iteration)
          (if (not= (mapv - (nth rocks check-idx) (nth rocks last-idx)) coord-delta)
            false
            (recur check-idx (+ check-idx cycle-size) false))))))

(defn check-cyle [rocks start-idx cycle-size]
  (loop [cur-start-idx start-idx]
    (if (>= cur-start-idx (+ start-idx cycle-size))
      true
      (if (check-cyle-point rocks cur-start-idx cycle-size)
        (recur (inc cur-start-idx))
        false))))

(defn find-cycle [rocks]
    (loop [start-idx 0
           cycle-size 1
           best nil]
      (if (>= start-idx (/ (count rocks) 2)) ;; do not accept cycles with only 1 iteration
        best
        (if (or
             (>= (+ start-idx cycle-size) (count rocks))
             (and (not(nil? best)) (>= cycle-size (:cycle-size best))))
          (recur (inc start-idx) (if (<= start-idx 1) (+ start-idx 2) (* start-idx 2)) best) ;; do not consider cycles where the idx delta is less than start-idx
          (if (check-cyle rocks start-idx cycle-size)
            (let [new-best (if (or (nil? best)
                                   (> (:cycle-size best) cycle-size))
                             {:start-idx start-idx
                              :cycle-size cycle-size
                              :start-coord (nth rocks start-idx)
                              :coord-delta (mapv - (nth rocks (+ start-idx cycle-size)) (nth rocks start-idx))}
                             best)]
              (recur start-idx (inc cycle-size) new-best))
            (recur start-idx (inc cycle-size) best))))))

(defn nth-height [input n]
  (let [n-idx (dec n)
        out (drop-rocks input 10000)
        rock-type (mod n-idx (count rocks))
        rock-type-idx (quot n-idx (count rocks))
        rocks (get-in out [:stopped-rocks rock-type])
        cycle-info (find-cycle rocks)
        cycle-count (quot (- rock-type-idx (:start-idx cycle-info)) (:cycle-size cycle-info))
        cycle-idx (mod (- rock-type-idx (:start-idx cycle-info)) (:cycle-size cycle-info))
        base-height (get-in out [:after-drop-heights rock-type (+ (:start-idx cycle-info) cycle-idx)])
        height (inc (+ base-height (* (second (:coord-delta cycle-info)) cycle-count)))]
    height))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (hydrate-moves (read-input (first args)))]
    (println "part 1:" (time (:height (drop-rocks input 2022))))
    (println "part 2:" (time (nth-height input 1000000000000)))))
