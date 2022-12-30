(ns advent-2022.day-16
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (reduce (fn [acc l] (let [parts (rest (re-find #"Valve (\S+) has flow rate=(\d+); tunnel(s?) lead(s?) to valve(s?) (.*)" l))]
                          (assoc acc (first parts) {:flow (Integer/parseInt (second parts))
                                                    :tunnels (str/split (nth parts 5) #", ")})))
            {} v)))

;; flood fill BFS. all edge weights are 1
(defn distances [input from]
  (loop [layer [from] out {from 0} d 1]
    (let [next-layer (filterv #(not (contains? out %)) (reduce concat (mapv #(:tunnels (input %)) layer)))
          next-out (reduce (fn [acc x] (assoc acc x d)) out next-layer)]
      (if (zero? (count next-layer))
        out
        (recur next-layer next-out (inc d))))))

(defn with-distances [input]
  (reduce (fn [acc k] (assoc-in acc [k :distances] (distances input k))) input (keys input)))

;; config is an input with :distances
;; state has keys: :t, :loc, :released, :release-rate, :closed-valves, :opened-valves
(defn next-states [config state max-t]
  (let [candidate-valves (as-> (:closed-valves state) v                                               ;; still closed
                           (filter #(> (get-in config [% :flow]) 0) v)                                ;; has flow
                           (filter #(<=                                                               ;; reachable (and openable) by max-t
                                     (+ (inc (:t state)) (get-in config [(:loc state) :distances %]))
                                     max-t) v))]
    (if (>= (:t state) max-t)
      [] ;; safety check for infinite loops
      (conj
       (mapv (fn [k]
               (let [dist-t (inc (get-in config [(:loc state) :distances k]))] ;; note the inc here. time spent travelling and then building
                 {:t (+ (:t state) dist-t)
                  :loc k
                  :released (+ (:released state) (* (:release-rate state) dist-t))
                  :release-rate (+ (:release-rate state) (get-in config [k :flow]))
                  :opened-valves (conj (:opened-valves state) k)
                  :closed-valves (disj (:closed-valves state) k)}))
             candidate-valves)
       {:t max-t
        :loc (:loc state)
        :released (+ (:released state) (* (:release-rate state) (- max-t (:t state))))
        :release-rate (:release-rate state)
        :opened-valves (:opened-valves state)
        :closed-valves (:closed-valves state)}))))

(defn init-state [config] {:t 0
                           :loc "AA"
                           :released 0
                           :release-rate 0
                           :opened-valves #{}
                           :closed-valves (set (filter #(> (get-in config [% :flow]) 0) (keys config)))})

(defn add-states [states candidates]
  (reduce (fn [acc c] (update-in acc [(:t c)] conj c)) states candidates))

(defn run [config max-t]
  (loop [states {0 (conj '() (init-state config))}
         fringe (vector (init-state config))
         iterations 1]
    (if (empty? fringe)
      {:states states
       :best (reduce #(if (or (> (:released %2) (:released %1))
                              (and (= (:released %2) (:released %1))
                                   (> (:release-rate %2) (:release-rate %1)))) %2 %1)
                     {:released -1 :release-rate -1} (states max-t))
       :iterations iterations}
      (let [cur (peek fringe)
            children (next-states config cur max-t)]
        (recur
         (add-states states children)
         (reduce conj (pop fringe) (filter #(< (:t %) max-t) children))
         (inc iterations))))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))
        config (with-distances input)]
    (let [round-count 30
          p1 (time (run config round-count))]
      (println "part 1:"
               (get-in p1 [:best :released])
               (format "(%s iterations)" (get-in p1 [:iterations]))
               (format "(%s max-t states)" (count (get-in p1 [:states round-count]))))
      (println "best")
      (pp/pprint (:best p1)))
    (println "part 2:" (time "TODO"))))
