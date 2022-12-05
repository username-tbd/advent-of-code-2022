(ns clj-aoc.day01
  (:require [clj-aoc.util :as u])
  (:gen-class))

;; Load data and convert: ("5" "" "13") => (5 nil 13)
(def calories
  (->> (u/load-lines 1)
       (replace {"" "nil"})
       (map read-string)))

(def elf-cal-totals
  (-> (fn [coll line-cals]
        (if (some? line-cals)
          (update coll (dec (count coll)) + line-cals) 
          (conj coll 0)))
      (reduce [0] calories)))

(println (apply max elf-cal-totals))
(println (->> elf-cal-totals sort (take-last 3) (apply +)))
