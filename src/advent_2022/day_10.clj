(ns advent-2022.day-10
  (:require [clojure.string :as str]))

(defn read-input [file-name]
  (map str/trim-newline (str/split (slurp file-name) #"\n")))

;; organized so that every iteration ticks t++
(defn run-cycles [cmds]
  (loop [state {:t 1 :v 1}
         cmds cmds
         interesting 0]
    ;; (println state cmds interesting)
    (let [cmd (first cmds)
          interesting (if (= 0 (mod (- (:t state) 20) 40))
                        (+ interesting (* (:t state) (:v state)))
                        interesting)]
      (if (nil? cmd)
        {:state state :interesting interesting}
        (cond
          (= cmd "noop")
          (recur {:t (inc (:t state)) :v (:v state)} (rest cmds) interesting)

          (str/starts-with? cmd "addx ") ;; put x to the top of the stack to add in the next tick
          (let [x (Integer/parseInt (second (str/split cmd #"addx ")))]
            (recur {:t (inc (:t state)) :v (:v state)} (conj (rest cmds) x) interesting))

          :else ;; add number pushed by addx
          (recur {:t (inc (:t state)) :v (+ (:v state) cmd)} (rest cmds) interesting))))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (run-cycles input))
    (println "part 2:" "TODO")))
