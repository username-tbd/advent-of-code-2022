(ns clj-aoc.day06
  (:require [clj-aoc.util :as u])
  (:gen-class))

(loop [stream (first (u/load-lines 6))
       ind 4]
  (if (= 4 (count (distinct (take 4 stream))))
    ind
    (recur (rest stream) (inc ind))))

(loop [stream (first (u/load-lines 6))
       ind 14]
  (if (= 14 (count (distinct (take 14 stream))))
    ind
    (recur (rest stream) (inc ind))))
