(ns advent-2022.day-19
  (:require [clojure.string :as str]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (mapv (fn [l] (let [parts (map #(Integer/parseInt %)
                                   (rest (re-find #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian." l)))]
                    {:id (nth parts 0)
                     ;; robot -> costs
                     :costs {:ore {:ore (nth parts 1)}
                             :clay {:ore (nth parts 2)}
                             :obsidian {:ore (nth parts 3) :clay (nth parts 4)}
                             :geode {:ore (nth parts 5) :obsidian (nth parts 6)}}})) v)))

(def initial-state {:t 0
                    :resources {:ore 0 :clay 0 :obsidian 0 :geode 0}
                    :robots {:ore 1 :clay 0 :obsidian 0 :geode 0}
                    :building nil})

(defn build?
  "returns new state if can build this robot type for the state, else return nil"
  [robot-type blueprint state]
  (let [costs (get-in blueprint [:costs robot-type])]
    (when (every? (fn [[k v]] (>= (get-in state [:resources k]) v)) costs)
      (as-> state s
        (assoc s :building robot-type)
        (reduce (fn [acc [k v]]
                  (update-in acc [:resources k] #(- % v)))
                s costs)
        (assoc s :prior state)))))

(defn add-states
  "add candidate states to states data"
  [states candidates]
  (reduce (fn [acc c] (update-in acc [(:t c)] conj c)) states candidates))

(defn best-possible
  "upper bound of geodes possible from this state"
  [max-t state]
  (let [t-left (- max-t (:t state))]
    (+ (get-in state [:resources :geode] 0)
       (* t-left (get-in state [:robots :geode] 0))
       (/ (* t-left (dec t-left)) 2))))

(defn run [max-t blueprint]
  (loop [states {0 (conj '() initial-state)} ;; all visited states keyed by round number
         stack [initial-state]               ;; stack for DFC
         best-so-far 0                       ;; most geodes we have seen so far
         state-count 1]                      ;; total number of states visited
    (cond
      (empty? stack) {:states states
                      :best (reduce #(if (> (get-in %2 [:resources :geode]) (get-in %1 [:resources :geode] 0)) %2 %1) (states max-t))
                      :n state-count}
      ;; branch and bound based on best possible future geode count from the current state
      (< (best-possible max-t (peek stack)) best-so-far) (recur states (pop stack) best-so-far (inc state-count))
      :else (let [cur (peek stack)
                  best-so-far (max best-so-far (get-in cur [:resources :geode] 0))
                  buildable (if (<= (- max-t (:t cur)) 1)
                              [] ;; do not build on second to last round
                              (->> [:ore :clay :obsidian :geode]
                                   ;; do not build a robot if we could have built it in the previous turn and built nothing
                                   (filter #(or (nil? (:prior cur))
                                                (not (and (build? % blueprint (:prior cur))
                                                          (nil? (get-in cur [:prior :building]))))))
                                   ;; do not build more robots than the max amount of that resource we need in 1 round
                                   (filter #(let [max-cost (reduce (fn [acc v]
                                                                     (+ acc (get-in (second v) [%] 0)))
                                                                   0 (seq (:costs blueprint)))]
                                              (or (zero? max-cost)
                                                  (< (get-in cur [:resources %] 0) max-cost))))))
                  candidates (if (= (:t cur) max-t)
                               []
                               (as-> [] s
                                 ;; 1. taking a time step. getting resources and completing builds
                                 (conj s (as-> cur v
                                           (update-in v [:t] + 1)
                                           (update-in v [:resources] #(merge-with + % (:robots v)))
                                           (if (:building v)
                                             (update-in v [:robots (:building v)] + 1)
                                             v)
                                           (assoc v :building nil)
                                           (assoc v :prior cur)))
                                 ;; 2. build a new robot
                                 (if (nil? (:building cur))
                                   (reduce (fn [acc c]
                                             (let [new-state (build? c blueprint cur)]
                                               (if new-state
                                                 (conj acc new-state)
                                                 acc)))
                                           s buildable)
                                   s)))]
              (recur (add-states states candidates) (vec (concat (pop stack) candidates)) best-so-far (inc state-count))))))

(defn print-build-order [state]
  (loop [cur state prev nil]
    (when (or (nil? prev) (not= (:t cur) (:t prev)))
      (if (nil? (:building cur))
        (println (:t cur) "nop")
        (println (:t cur) (:building cur))))
    (when-not (nil? (:prior cur)) (recur (:prior cur) cur))))

(defn geode-count [max-t bp]
  (let [{states :states state-count :n best :best} (time (run max-t bp))]
    ;; (print-build-order best)
    (println "id:" (:id bp) "max-t:" max-t "geodes:" (get-in best [:resources :geode] 0) "final round states:" (count (states max-t)) "total states:" state-count)
    (get-in best [:resources :geode] 0)))

(defn quality-level [bp]
  (let [max-t 24
        geodes (geode-count max-t bp)]
    (* (:id bp) geodes)))

(defn quality-levels [bps]
  (reduce + (map quality-level bps)))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (time (quality-levels input)))
    (println "part 2:" (time (reduce * (map #(geode-count 32 %) (subvec input 0 (min 3 (count input)))))))))
