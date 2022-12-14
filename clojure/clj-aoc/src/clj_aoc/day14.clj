(ns clj-aoc.day14
  (:require [clj-aoc.util :as u])
  (:gen-class))

(defn text-line->coord-line [line]
  (->> (clojure.string/split line #" -> ")
       (mapv #(clojure.string/split % #","))
       (mapv #(mapv read-string %))))

;; Assumes one of either x or y will be constant!
(defn fill-out-coord-pair [pair]
  (let [xs (map first pair)
        ys (map second pair)] 
    (for [x (range (apply min xs) (inc (apply max xs)))
          y (range (apply min ys) (inc (apply max ys)))]
      [x y])))

(defn coord-line->rocks [coord-line]
  (let [coord-pairs (partition 2 1 coord-line)]
    (->> (mapv fill-out-coord-pair coord-pairs)
         (reduce into [])
         distinct))) ; There are "backtracks" in the input data

(def rocks
  (->> (u/load-lines 14)
       (mapv (comp coord-line->rocks text-line->coord-line))
       (reduce into [])))
