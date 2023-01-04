(ns advent-2022.day-24
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(def verbose false)
(def t-limit -1)

(defn read-input [file-name]
  (let [file (slurp file-name)
        lines (str/split file #"\n")
        blizzards (->> (map-indexed (fn [y l]
                                      (keep-indexed (fn [x v]
                                                      (when (and (not= v ".") (not= v "#"))
                                                        {:loc [y x]
                                                         :dir (case v
                                                                ">" [0 1]
                                                                "<" [0 -1]
                                                                "v" [1 0]
                                                                "^" [-1 0]
                                                                (throw (Exception. (format "unexpected symbol %s" v))))}))
                                                    (str/split l #"")))
                                    lines)
                       (reduce concat [])
                       (reduce #(update %1 (:loc %2) conj %2) {}))]
    {:config {:height (count lines)
              :width (count (first lines))
              :start [0 1]
              :end [(dec (count lines)) (- (count (first lines)) 2)]}
     :loc [0 1]
     :blizzards blizzards
     :t 0}))

;; in bounds and not on top of a wall
(def free-loc? (memoize (fn [config loc]
                          (or (= loc (:start config))
                              (= loc (:end config))
                              (and (pos? (first loc))
                                   (< (first loc) (dec (:height config)))
                                   (pos? (second loc))
                                   (< (second loc) (dec (:width config))))))))

(defn move-blizzard [config blizzard]
  (let [next-loc (mapv + (:dir blizzard) (:loc blizzard))
        next-loc (if (free-loc? config next-loc)
                   next-loc
                   (loop [cur (case (:dir blizzard)
                                [0 1] [(first next-loc) 0]
                                [0 -1] [(first next-loc) (dec (:width config))]
                                [1 0] [0 (second next-loc)]
                                [-1 0] [(dec (:height config)) (second next-loc)])]
                     (if (free-loc? config cur)
                       cur
                       (recur (mapv + (:dir blizzard) cur)))))]
    (assoc blizzard :loc next-loc)))

(defn move-blizzards [config blizzards]
  (reduce (fn [acc v]
            (reduce #(let [next (move-blizzard config %2)]
                       (update %1 (:loc next) conj next)) acc (second v))) {} blizzards))

(def moves [[0 1] [1 0] [0 -1] [-1 0] [0 0]])

(def blizzards-at-t (memoize (fn [config blizzards t]
                               (if (zero? t)
                                 blizzards
                                 (move-blizzards config (blizzards-at-t config blizzards (dec t)))))))

(defn travel [{config :config loc :loc blizzards :blizzards t :t}]
  (loop [fringe (conj clojure.lang.PersistentQueue/EMPTY {:t t :loc loc})
         seen #{{:t t :loc loc}}
         iters 0]
    (if (empty? fringe)
      (do (when verbose (println "iters:" iters)) nil)
      (let [{t :t
             loc :loc} (peek fringe)
            cur (peek fringe)
            next-blizzards (blizzards-at-t config blizzards (inc t))]
        (when verbose (pp/pprint cur))
        (if (= loc (:end config))
          (do (when verbose (println "iters:" iters)) (:t cur))
          (if (and (pos? t-limit) (>= t t-limit))
            (recur (pop fringe) seen (inc iters))
            (let [next-locs (as-> moves vs
                              (mapv #(mapv + loc %) vs)
                              (filter #(free-loc? config %) vs)
                              (filter #(not (contains? next-blizzards %)) vs))
                  new-states (as-> (mapv #(identity {:t (inc t) :loc %}) next-locs) ss
                               (filter #(not (contains? seen %)) ss))
                  new-fringe (reduce conj (pop fringe) new-states)
                  new-seen (reduce conj seen new-states)]
              (recur new-fringe new-seen (inc iters)))))))))

(defn travel-twice [input]
  (let [{config :config
         blizzards :blizzards} input
        there (travel input)
        back (travel {:config (as-> config c
                                (assoc c :start (:end config))
                                (assoc c :end (:start config)))
                      :loc (:end config)
                      :blizzards blizzards
                      :t there})
        there-again (travel {:config config
                             :loc (:start config)
                             :blizzards blizzards
                             :t back})]
    there-again))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (time (travel input)))
    (println "part 2:" (time (travel-twice input)))))
