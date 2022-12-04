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

(defn either-contained? [x]
  (let [contained-in?
        (fn [p q] (and (>= (first p) (first q)) (<= (last p) (last q))))]
    (or (contained-in? (first x) (last x))
        (contained-in? (last x) (first x)))))

(defn overlap? [x]
  (and (>= (second (first x)) (first (second x)))
       (<= (first (first x)) (second (second x)))))

(defn count-assignments [pred lines]
  (apply + (map (comp {false 0 true 1} pred split-assignment) lines)))

(println (count-assignments either-contained? lines))
(println (count-assignments overlap? lines))
