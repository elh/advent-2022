(ns advent-2022.day-5
  (:require [clojure.string :as str]))

(defn build-stack
  "Given a stack index and the relevant input text, build a stack of crates (a Clojure list)"
  [i stack-lines]
  (letfn [(agg [crate-stack line]
            (let [v (subs line (+ 1 (* 4 i)) (+ 2 (* 4 i)))]
              (if (= " " v)
                crate-stack
                (conj crate-stack v))))]
    (reduce agg '() (reverse stack-lines)))) ;; reverse to load up the stacks from the bottom

(defn read-input
  "Read input and return a vector of crate stacks and a vector of move line strings"
  [file-name]
  (let [lines (str/split (slurp file-name) #"\n")
        sep-line (.indexOf lines "")
        move-lines (vec (subvec lines (+ sep-line 1) (count lines)))
        stack-lines (vec (subvec lines 0 (- sep-line 1))) ;; only the lines with crates
        stack-count (+ 1 (quot (count (nth lines 0)) 4))
        stacks (vec (map (fn [i] (build-stack i stack-lines)) (range stack-count)))]
    {:move-lines move-lines, :stacks stacks}))

(defn move-one-by-one
  "Given a step of the move procedure, update the stacks by pushing and popping crates one by one. A move strategy"
  [stacks {:keys [move-count from-stack to-stack]}]
  (loop [move-count move-count stacks stacks]
    (if (zero? move-count)
      stacks
      (let [moved (peek (nth stacks from-stack))
            stacks (-> stacks
                       (assoc to-stack (conj (nth stacks to-stack) moved))
                       (assoc from-stack (pop (nth stacks from-stack))))]
        (recur (- move-count 1) stacks)))))

(defn move-bulk
  "Given a step of the move procedure, update the stacks by pushing and popping all moving crates at once. A move strategy"
  [stacks {:keys [move-count from-stack to-stack]}]
  (let [moved (take move-count (nth stacks from-stack))
        stacks (-> stacks
                   (assoc to-stack (concat moved (nth stacks to-stack)))
                   (assoc from-stack (drop move-count (nth stacks from-stack))))]
    stacks))

(defn make-moves
  "Given a move strategy, apply it for all of the move steps in the input and return the final resulting stacks"
  [move-strategy {:keys [move-lines stacks]}]
  (loop [move-lines move-lines stacks stacks]
    (if (empty? move-lines)
      stacks
      (let [move-line (first move-lines)
            parts (map #(Integer/parseInt %) (rest (re-find #"move (\d+) from (\d+) to (\d+)" move-line)))
            move-data {:move-count (first parts)
                       :from-stack (- (second parts) 1) ;; 0 indexed
                       :to-stack (- (nth parts 2) 1)}   ;; 0 indexed
            stacks (move-strategy stacks move-data)]
        (recur (rest move-lines) stacks)))))

(defn top-crates
  "return the top crate from each stack as a string"
  [stacks]
  (str/join "" (map (fn [stack] (first stack)) stacks)))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))

  (let [input (read-input (first args))]
    (println "part 1:" (top-crates (make-moves move-one-by-one input)))
    (println "part 2:" (top-crates (make-moves move-bulk input)))))
