(ns advent-2022.day-10
  (:require [clojure.string :as str]))

(defn read-input [file-name]
  (map str/trim-newline (str/split (slurp file-name) #"\n")))

;; every iteration ticks t++. it makes seeing states very easy)
;; every command on the stack is either a noop, addx, or a number pushed by a preceding addx
(defn run-cycles [cmds]
  (loop [state {:t 1 :v 1}
         cmds cmds
         history [state]
         interesting 0]
    (let [cmd (first cmds)
          next {:t (inc (:t state)) :v (:v state)}
          interesting (if (= 0 (mod (- (:t state) 20) 40))
                        (+ interesting (* (:t state) (:v state)))
                        interesting)]
      (if (nil? cmd)
        {:state state :history history :interesting interesting}
        (cond
          (= cmd "noop")
          (recur next (rest cmds) (conj history next) interesting)

          (str/starts-with? cmd "addx ") ;; put x to the top of the stack to add in the next tick
          (let [x (Integer/parseInt (second (str/split cmd #"addx ")))]
            (recur next (conj (rest cmds) x) (conj history next) interesting))

          :else ;; add number pushed by addx
          (recur (update-in next [:v] + cmd) (rest cmds) (conj history (update-in next [:v] + cmd)) interesting))))))

(defn crt [data]
  (let [pixels (map (fn [v]
                      (if (<= (dec (:v v)) (mod (dec (:t v)) 40) (inc (:v v)))
                        "█"
                        " ")) data)]
       (str/join "\n" (map str/join (partition 40 pixels)))))

(defn -main [& args]
  (when (not= (count args) 1)
    (throw (Exception. (format "FAIL: expects input file as cmdline arg. got %d args" (count args)))))
  (let [input (read-input (first args))]
    (println "part 1:" (:interesting (run-cycles input)))
    (println "part 2:")
    (println (crt (:history (run-cycles input))))))
