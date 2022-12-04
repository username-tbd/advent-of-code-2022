(ns clj-aoc.day04
  (:require [clj-aoc.util :as u])
  (:gen-class))

(defn split-assignment [assignment]
  (->> assignment
       (re-find #"(\d+)-(\d+),(\d+)-(\d+)")
       rest
       (map read-string)
       (split-at 2)))

(def assignment-pairs (map split-assignment (u/load-lines 4)))

(defn containment? [x]
  (let [contained-in?
        (fn [p q] (and (>= (first p) (first q)) (<= (last p) (last q))))]
    (or (contained-in? (first x) (last x))
        (contained-in? (last x) (first x)))))

(defn overlap? [x]
  (and (>= (last (first x)) (first (last x)))
       (<= (first (first x)) (last (last x)))))

(println (count (filter containment? assignment-pairs)))
(println (count (filter overlap? assignment-pairs)))
