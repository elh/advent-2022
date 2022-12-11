(ns advent-2022.day-07
  (:require [clojure.string :as str]))

(defn read-input
  [file-name]
  (vec (map str/trim-newline (str/split (slurp file-name) #"\n"))))

(defn dir-name [cur child]
  (if (= "/" cur) (str "/" child) (str cur "/" child)))

(defn process-history
  "Parse lines into a map of dirs and their direct sizes and child dirs."
  [lines]
  (:dirs
   (reduce (fn [acc line]
             (cond
               (str/starts-with? line "$ ls")
               acc ;; ignore

               (str/starts-with? line "$ cd ")
               (let [new-dir (str/replace-first line #"\$ cd " "")]
                 (case new-dir
                   "/" (assoc acc :cur-dir "/")
                   ".." (update-in acc [:cur-dir] str/replace #"/[a-zA-Z]+$" "")
                   (update-in acc [:cur-dir] dir-name new-dir)))

               :else
               (if (str/starts-with? line "dir")
                 (update-in acc [:dirs (:cur-dir acc) :children] conj (dir-name (:cur-dir acc) (str/replace-first line #"dir " "")))
                 (let [file-size (Integer/parseInt (first (str/split line #" ")))]
                   (update-in acc [:dirs (:cur-dir acc) :size] #(+ file-size (if (some? %) % 0)))))))
           {:dirs {} :cur-dir "/"}
           lines)))

(defn calculate-total-size [dirs dir-name]
  (+ (get-in dirs [dir-name :size] 0)
     (reduce (fn [acc child]
               (+ acc (calculate-total-size dirs child)))
             0
             (get-in dirs [dir-name :children]))))

(defn calculate-total-sizes
  "Return map of dirs with their total, indirect file sizes."
  [dirs]
  (into {} (for [[k v] dirs] [k (assoc v :total-size (calculate-total-size dirs k))])))

(defn find-smallest-dir-to-delete [dirs]
  (let [total-space 70000000
        required-space 30000000
        used-space (get-in dirs ["/" :total-size])
        sorted-by-size (sort-by (fn [dir] (get-in dir [1 :total-size]))
                                <
                                dirs)
        smallest-dir (filter (fn [dir]
                               (< required-space (+ (get-in dir [1 :total-size]) (- total-space used-space))))
                             sorted-by-size)]
    (:total-size (second (first smallest-dir)))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (reduce (fn [acc dir]
                                 (if (< (:total-size (second dir)) 100000)
                                   (+ acc (:total-size (second dir)))
                                   acc))
                               0
                               (calculate-total-sizes (process-history input))))
    (println "part 2:" (->> input
                            process-history
                            calculate-total-sizes
                            find-smallest-dir-to-delete))))
