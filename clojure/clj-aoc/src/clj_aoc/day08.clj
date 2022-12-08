(ns clj-aoc.day08
  (:require [clj-aoc.util :as u])
  (:gen-class))

(defn line-to-tree-mat [line]
  (map #(read-string (str %)) line))

(def tree-mat
  (map line-to-tree-mat (u/load-lines 8)))

(defn viz-line [tree-line]
  "Returns a boolean seq that indicates tree visibility, reading left to right.
  (023151) => (true true true false true false)"
  (loop [trees tree-line
         tallest-seen -1
         vis-vec []]
    (let [tree (first trees)]
      (if (nil? tree)
        vis-vec
        (recur (rest trees)
               (max tree tallest-seen)
               (conj vis-vec (> tree tallest-seen)))))))

(defn transpose [mat]
  (let [grab-slice (fn [ind] (map #(nth % ind) mat))]
    (map grab-slice (range 99))))

(def vis-from-left
  (->> tree-mat (map viz-line)))
(def vis-from-right
  (->> tree-mat (map reverse) (map viz-line) (map reverse)))
(def vis-from-top
  (->> tree-mat transpose (map viz-line) transpose))
(def vis-from-bottom
  (->> tree-mat transpose (map reverse) (map viz-line) (map reverse) transpose))

(def flat-visibilities
  (map flatten [vis-from-left vis-from-right vis-from-top vis-from-bottom]))

(defn combine-flat-vis [v1 v2]
  (map #(or %1 %2) v1 v2))

(->> (reduce combine-flat-vis flat-visibilities)
     (map {false 0 true 1})
     (apply +))

