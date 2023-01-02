(ns advent-2022.day-25-gen
  (:require [advent-2022.day-25 :as day-25]
            [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn read-input [file-name]
  (as-> (slurp file-name) v
    (str/split v #"\n")))

(defn without-leading-zeros [v]
  (let [out (clojure.string/replace v #"^0+" "")]
    (if (empty? out)
      "0"
      out)))

;; create all permutations of snafu numbers with n snafu digits
(defn snafu-perms [n-digits]
  (letfn [(snafu-perms-recur [n-digits]
            (if (zero? n-digits)
              [""]
              (reduce (fn [acc v]
                        (concat acc (mapv #(str v %) (snafu-perms-recur (dec n-digits)))))
                      [] (keys day-25/snafu-symbols))))]
    (map without-leading-zeros (snafu-perms-recur n-digits))))

;; generate a mapping of the first n snafu numbers by their decimal values
(defn generate-snafu [n-digits]
  (let [perms (snafu-perms n-digits)]
    (reduce (fn [acc v]
              (let [dec (day-25/snafu-to-dec v)]
                (if (neg? dec)
                  acc
                  (assoc acc v dec))))
            (priority-map)
            perms)))

;; a test-dec-to-snafu assert-fn
(defn pp-test-dec-to-snafu [n expected actual]
  (println (if (= actual expected) "✅" "❌") (format "n=%d expected=\"%s\" actual=\"%s\"" n expected actual)))

;; a helper for tests but can also be run directly with prettier compact reporting
(defn test-dec-to-snafu [max assert-fn]
  (let [snafu-to-dec (generate-snafu 5)]
    (loop [cur 0 snafu-to-dec snafu-to-dec]
      (when-not (> cur max)
        (when (empty? snafu-to-dec) (throw (Exception. "generated test data is too small")))
        (let [actual (day-25/dec-to-snafu cur) expected (ffirst snafu-to-dec)]
          (assert-fn cur expected actual)
          (recur (inc cur) (rest snafu-to-dec)))))))

(defn test-back-to-back [reqs]
  (loop [reqs reqs]
    (when-not (empty? reqs)
      (let [s (first reqs) d (day-25/snafu-to-dec s) back-to-s (day-25/dec-to-snafu d) back-to-d (day-25/snafu-to-dec back-to-s)]
        (println (format "%s s::%s d::%d back-to-s::%s back-to-d::%d" (= s back-to-s) s d back-to-s back-to-d))
        (recur (rest reqs))))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (test-dec-to-snafu 63 pp-test-dec-to-snafu)
    (test-back-to-back input)))
