(ns clj-aoc.day04
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def lines
  (u/load-lines "../../inputs/input-04.txt"))

(defn split-assignment [assignment]
  (->> (re-find #"((\d+)-(\d+)),((\d+)-(\d+))" assignment)
       (#(map % [2 3 5 6]))
       (map read-string)
       (split-at 2)))

(defn full-contain? [x]
  (or (and (>= (first (first x)) (first (second x)))
           (<= (second (first x)) (second (second x))))
      (and (<= (first (first x)) (first (second x)))
           (>= (second (first x)) (second (second x))))))

(defn overlap? [x]
  (not (or (< (second (first x)) (first (second x)))
           (> (first (first x)) (second (second x))))))

(defn count-assignments [pred lines]
  (apply + (map (comp {false 0 true 1} pred split-assignment) lines)))

(println (count-assignments full-contain? lines))
(println (count-assignments overlap? lines))
