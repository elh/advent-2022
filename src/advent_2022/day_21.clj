(ns advent-2022.day-21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")
    (map (fn [l] (rest (re-find #"(\S+): (.*)" l))) v)
    (reduce (fn [acc x]
              (if (str/includes? (second x) " ")
                (let [parts (rest (re-find #"(\S+) (\S+) (\S+)" (second x)))]
                  (assoc acc (first x) {:op (eval (read-string (second parts))) :args [(first parts) (nth parts 2)]}))
                (assoc acc (first x) {:value (Integer/parseInt (second x))})))
            {} v)))

(defn get-value [input x]
  (if (string? x)
    (let [v (input x)]
      (or
       (:value v)
       ((:op v) (get-value input (get-in v [:args 0])) (get-value input (get-in v [:args 1])))))
    ((first x) (get-value input (second x)) (get-value input (nth x 2)))))

;; ys is a set
(defn get-one-in-ys [xs ys] (first (filter #(contains? ys %) xs)))
(defn get-one-not-in-ys [xs ys] (first (filter #(not (contains? ys %)) xs)))

(defn depends-on [input x]
  (loop [out #{x} queue [x]]
    (if (empty? queue)
      out
      (let [cur (first queue)
            directs (reduce (fn [acc k]
                              (if (some #{cur} (get-in input [k :args] []))
                                (conj acc k)
                                acc))
                            #{} (keys input))]
        (recur (set/union out directs) (concat (rest queue) directs))))))

(defn solve [input equation unbound]
  (let [depends-on-unbound (depends-on input unbound)]
    (loop [lhs (get-one-in-ys (:args equation) depends-on-unbound)
           rhs (get-one-not-in-ys (:args equation) depends-on-unbound)]
      (if (= lhs unbound)
        (get-value input rhs)
        (let [lhs-expr (input lhs)
              lhs-arg (get-one-in-ys (:args lhs-expr) depends-on-unbound)
              other-arg (get-one-not-in-ys (:args lhs-expr) depends-on-unbound)
              next-rhs (condp = (:op lhs-expr)
                         + [- rhs other-arg]
                         * [/ rhs other-arg]
                         - (if (= lhs-arg (first (:args lhs-expr)))
                             [+ rhs other-arg]
                             [- other-arg rhs])
                         / (if (= lhs-arg (first (:args lhs-expr)))
                             [* rhs other-arg]
                             [/ other-arg rhs])
                         (throw (Exception. "unsupported op")))]
          (recur lhs-arg next-rhs))))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (time (get-value input "root")))
    (println "part 2:" (time (solve input (input "root") "humn")))))
