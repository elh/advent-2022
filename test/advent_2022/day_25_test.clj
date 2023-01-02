(ns advent-2022.day-25-test
  (:require [advent-2022.day-25-gen :as day-25-gen]
            [clojure.test :refer [deftest is]]))

(deftest dec-to-snafu-test
  (let [up-to 1000]
    (day-25-gen/test-dec-to-snafu up-to
                                  (fn [n expected actual]
                                    (is (= expected actual) (format "❌ n=%d expected=\"%s\" actual=\"%s\"" n expected actual))))))
