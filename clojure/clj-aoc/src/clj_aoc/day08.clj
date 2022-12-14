(ns clj-aoc.day08
  (:require [clj-aoc.util :as u])
  (:gen-class))

(defn line-to-tree-mat [line]
  (map #(read-string (str %)) line))

(def tree-mat
  (map line-to-tree-mat (u/load-lines 8)))

(defn transpose [mat]
  (let [distinct-row-lens (distinct (map count mat)) 
        get-col (fn [ind] (map #(nth % ind) mat))]
    (if (= 1 (count distinct-row-lens))
      (map get-col (range (first distinct-row-lens))))))

(defn build-vis-line [tree-line]
  "Builds a boolean vec corresponding to left-to-right visibility.
  (0 2 1 5) => [true true false true]"
  (loop [trees tree-line
         tallest-seen -1
         vis-vec []]
    (let [tree (first trees)]
      (if (nil? tree)
        vis-vec
        (recur (rest trees)
               (max tree tallest-seen)
               (conj vis-vec (> tree tallest-seen)))))))

(defn build-mat [{:keys [tree-mat line-fn view-direction]}]
  (let [line-fn-reverse 
        (fn [tree-line] ((comp reverse line-fn reverse) tree-line))]
    (condp = view-direction
      :right (map line-fn tree-mat)
      :left (map line-fn-reverse tree-mat)
      :down (->> (transpose tree-mat)
                 (map line-fn)
                 transpose)
      :up (->> (transpose tree-mat)
               (map line-fn-reverse)
               transpose))))

(def vis-mats
  (map #(build-mat {:tree-mat tree-mat
                    :line-fn build-vis-line
                    :view-direction %})
       [:right :left :down :up]))

(def flat-vis-seqs
  (map flatten vis-mats))

(defn combine-flat-vis-seqs [v1 v2]
  (map #(or %1 %2) v1 v2))

(->> (reduce combine-flat-vis-seqs flat-vis-seqs)
     (map {false 0 true 1})
     (apply +)
     println)

;; -----------
;; Part Two

(defn calc-view [tree-line]
  "Calculate the viewing distance from the first element.
  (5 2 3 5 1) => 3; (1 5 0 1) => 1; (3) => 0"
  (let [tree (first tree-line)]
    (loop [view-line (rest tree-line)
           view-distance 0]
      (cond
        (empty? view-line) view-distance
        (>= (first view-line) tree) (inc view-distance)
        :else (recur (rest view-line) (inc view-distance))))))

(defn build-views-line [tree-line]
  (loop [trees tree-line
         views-vec []]
    (let [tree (first trees)]
      (if (nil? tree)
        views-vec
        (recur (rest trees)
               (conj views-vec (calc-view trees)))))))

(def views-mats
  (map #(build-mat {:tree-mat tree-mat
                    :line-fn build-views-line
                    :view-direction %})
       [:right :left :down :up]))

(def flat-views-seqs
  (map flatten views-mats))

(def viewing-distance-scores
  (apply #(map * %1 %2 %3 %4) flat-views-seqs))

(println (apply max viewing-distance-scores))
