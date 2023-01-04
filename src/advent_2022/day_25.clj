(ns advent-2022.day-25
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")))

(def snafu-symbols {"2" 2 "1" 1 "0" 0 "-" -1 "=" -2})
(def snafu-symbols-rev (set/map-invert snafu-symbols))

(defn as-digits [str]
  (if (= str "")
    [0]
    (as-> str v
      (str/split v #"")
      (mapv #(if (nil? (snafu-symbols %))
               (throw (Exception. "unexpected SNAFU digit"))
               (snafu-symbols %)) v))))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn snafu-to-dec [s]
  (as-> s v
    (as-digits v)
    (map-indexed (fn [idx itm]
                   (*
                    (exp 5 (- (dec (count v)) idx))
                    itm))
                 v)
    (reduce + v)))

(defn snafu-digits [d]
  (loop [out 1 last 0]
    (let [digit-max (+ last (* 2 (exp 5 (dec out))))]
      (if (<= d digit-max)
        out
        (recur (inc out) digit-max)))))

(defn dec-to-snafu [d]
  (loop [out ""
         digits (snafu-digits d)
         d d]
    (if (zero? digits)
      (if (empty? out)
        "0"
        out)
      ;; NOTE: clojure.math.numeric-tower/abs required because of integer overflow
      (let [closest (apply min-key #(math/abs (- d (* % (exp 5 (dec digits))))) (keys snafu-symbols-rev))
            digit-value (* closest (exp 5 (dec digits)))
            new-snafu (snafu-symbols-rev closest)]
        (recur (str out new-snafu) (dec digits) (- d digit-value))))))

;; part 1

(defn fuel [reqs]
  (let [total (reduce (fn [acc v]
                        (+ acc (snafu-to-dec v))) 0 reqs)]
    (dec-to-snafu total)))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (time (fuel input)))
    (println "part 2:" (time "🤫🎄"))))
