(ns clj-aoc.day01-improved
  (:require [clj-aoc.util :as u])
  (:gen-class))

;; Load data and convert: ("5" "" "13") => (5 nil 13)
(def calories
  (->> (u/load-lines 1)
       (replace {"" "nil"})
       (map read-string)))

(def elf-cal-totals
  (->> calories
       (partition-by nil?)
       (map #(apply + %))
       (remove nil?)))

(println (apply max elf-cal-totals))
(println (->> (sort elf-cal-totals) (take-last 3) (apply +)))
