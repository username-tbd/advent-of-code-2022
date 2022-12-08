(ns clj-aoc.day08
  (:require [clj-aoc.util :as u])
  (:gen-class))

(defn line-to-tree-mat [line]
  (map #(read-string (str %)) line))

(def tree-mat
  (map line-to-tree-mat (u/load-lines 8)))

(defn get-visibility-vec [tree-line]
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

(def tree-mat-T (transpose tree-mat))

(def vis-from-left (mapv get-visibility-vec tree-mat))
(def vis-from-right
  (->> (mapv reverse tree-mat)
       (mapv get-visibility-vec)
       (mapv reverse)
       (mapv vec)))
(def vis-from-top
  (->> (mapv get-visibility-vec tree-mat-T)
       transpose
       (mapv vec)))
(def vis-from-bottom
  (->> (mapv reverse tree-mat-T)
       (mapv get-visibility-vec)
       (mapv reverse)
       transpose
       (mapv vec)))

(def flat-visibilities
  (mapv flatten [vis-from-left vis-from-right vis-from-top vis-from-bottom]))

(defn combine-flat-vis [v1 v2]
  (mapv #(or %1 %2) v1 v2))

(->> (reduce combine-flat-vis flat-visibilities)
     (mapv {false 0 true 1})
     (apply +))


