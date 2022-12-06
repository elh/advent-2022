(ns advent-2022.day-5
  (:require [clojure.string :as str]))

(defn build-stack
  "Given a stack index and the relevant input text, builds a stack of crates (a Clojure list)."
  [i stack-lines]
  (letfn [(agg [crate-stack line]
            (let [v (subs line (inc (* 4 i)) (+ 2 (* 4 i)))]
              (if (= " " v)
                crate-stack
                (conj crate-stack v))))]
    (reduce agg '() (reverse stack-lines)))) ;; reverse to load up the stacks from the bottom

(defn read-input
  "Reads input and returns a vector of crate stacks and a vector of move line strings."
  [file-name]
  (let [lines (str/split (slurp file-name) #"\n")
        sep-line (.indexOf lines "")
        move-lines (vec (subvec lines (inc sep-line) (count lines)))
        stack-lines (vec (subvec lines 0 (dec sep-line))) ;; only the lines with crates
        stack-count (inc (quot (count (nth lines 0)) 4))
        stacks (vec (map (fn [i] (build-stack i stack-lines)) (range stack-count)))]
    {:move-lines move-lines, :stacks stacks}))

(defn move-one-by-one
  "A move strategy.
   Given a step of the move procedure, updates the stacks by pushing and popping crates one by one."
  [stacks {:keys [move-count from-stack to-stack]}]
  (loop [move-count move-count stacks stacks]
    (if (zero? move-count)
      stacks
      (let [moved (peek (nth stacks from-stack))
            stacks (-> stacks
                       (assoc to-stack (conj (nth stacks to-stack) moved))
                       (assoc from-stack (pop (nth stacks from-stack))))]
        (recur (dec move-count) stacks)))))

(defn move-bulk
  "A move strategy.
   Given a step of the move procedure, updates the stacks by pushing and popping all moving crates at once."
  [stacks {:keys [move-count from-stack to-stack]}]
  (let [moved (take move-count (nth stacks from-stack))
        stacks (-> stacks
                   (assoc to-stack (concat moved (nth stacks to-stack)))
                   (assoc from-stack (drop move-count (nth stacks from-stack))))]
    stacks))

(defn make-moves
  "Given a move strategy, applies it for all of the move steps in the input and returns the final resulting stacks."
  [move-strategy {:keys [move-lines stacks]}]
  (loop [move-lines move-lines stacks stacks]
    (if (empty? move-lines)
      stacks
      (let [move-line (first move-lines)
            parts (map #(Integer/parseInt %) (rest (re-find #"move (\d+) from (\d+) to (\d+)" move-line)))
            move-data {:move-count (first parts)
                       :from-stack (dec (second parts)) ;; 0 indexed
                       :to-stack (dec (nth parts 2))}   ;; 0 indexed
            stacks (move-strategy stacks move-data)]
        (recur (rest move-lines) stacks)))))

(defn top-crates
  "Returns the top crate from each stack as a string."
  [stacks]
  (str/join "" (map first stacks)))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))

  (let [input (read-input (first args))]
    (println "part 1:" (top-crates (make-moves move-one-by-one input)))
    (println "part 2:" (top-crates (make-moves move-bulk input)))))
