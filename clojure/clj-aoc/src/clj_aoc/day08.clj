(ns clj-aoc.day08
  (:require [clj-aoc.util :as u])
  (:gen-class))

(defn line-to-tree-mat [line]
  (map #(read-string (str %)) line))

(def tree-mat
  (map line-to-tree-mat (u/load-lines 8)))


(defn vis-line [{:keys [tree-line view-from]}]
  (let [fn-body
        (fn [tree-line reverse?]
          (loop [trees (if reverse? (reverse tree-line) tree-line )
                 tallest-seen -1
                 vis-vec []]
            (let [tree (first trees)]
              (if (nil? tree)
                (if reverse? (vec (reverse vis-vec)) vis-vec)
                (recur (rest trees)
                       (max tree tallest-seen)
                       (conj vis-vec (> tree tallest-seen)))))))]
    (cond (= view-from :left) (fn-body tree-line false)
          (= view-from :right) (fn-body tree-line true)
          :else nil)))

(defn transpose [mat]
  (let [grab-slice (fn [ind] (map #(nth % ind) mat))]
    (map grab-slice (range 99))))

(def vis-from-left (map #(vis-line {:tree-line % :view-from :left}) tree-mat))
(def vis-from-right (map #(vis-line {:tree-line % :view-from :right}) tree-mat))
(def vis-from-top
  (->> (transpose tree-mat)
       (map #(vis-line {:tree-line % :view-from :left}))
       transpose))
(def vis-from-bottom
  (->> (transpose tree-mat)
       (map #(vis-line {:tree-line % :view-from :right}))
       transpose))

(def flat-visibilities
  (map flatten [vis-from-left vis-from-right vis-from-top vis-from-bottom]))

(defn combine-flat-vis [v1 v2]
  (map #(or %1 %2) v1 v2))

(->> (reduce combine-flat-vis flat-visibilities)
     (map {false 0 true 1})
     (apply +))

