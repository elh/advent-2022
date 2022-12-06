(ns advent-2022.day-2
  (:require [clojure.string :as str]))

(defn read-input
  [file-name]
  (vec (map str/trim-newline (str/split (slurp file-name) #"\n"))))

(defn parse-line
  "Returns a tuple that represents the hands thrown."
  [line]
  (let [parts (str/split line #"\s+")
        theirsNum (case (first parts)
                    "A" 0
                    "B" 1
                    "C" 2)
        mineNum (case (second parts)
                  "X" 0
                  "Y" 1
                  "Z" 2)
        mineStrategy (case (second parts)
                       "X" "lose"
                       "Y" "draw"
                       "Z" "win")]
    {:theirs theirsNum, :mine mineNum, :mineStrategy mineStrategy}))

(defn adjust-for-strategy
  "Returns a new round with an updated :mine based on the :mineStrategy and :theirs."
  [round]
  (let [new-mine (case (:mineStrategy round)
                   "lose" (mod (dec (:theirs round)) 3)
                   "draw" (:theirs round)
                   "win" (mod (inc (:theirs round)) 3))]
    (assoc round :mine new-mine)))

(defn score-round
  "Returns a score incorporating the points from your hand and the result of the RPS round."
  [round]
  (+
   (inc (:mine round))                                      ;; points for your hand
   (cond (= (:theirs round) (:mine round)) 3                ;; draw
         (= (mod (inc (:theirs round)) 3) (:mine round)) 6  ;; win
         :else 0)))                                         ;; loss

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))

  (let [input (read-input (first args))]
    (println "part 1:" (->> input
                            (map parse-line)
                            (map score-round)
                            (reduce +)))
    (println "part 2:" (->> input
                            (map parse-line)
                            (map adjust-for-strategy)
                            (map score-round)
                            (reduce +)))))
