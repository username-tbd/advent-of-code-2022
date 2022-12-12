(ns clj-aoc.day12
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def lines (vec (u/load-lines 12)))
(def nrow (count lines))
(def ncol (count (first lines)))

(defn find-char-ind [character]
  (first
    (for [row (range nrow)
          col (range ncol)
          :when (= character (get-in lines [row col]))]
      [row col])))

(def start-ind (find-char-ind \S))
(def end-ind (find-char-ind \E))

(defn char->elevation [character]
  (cond
    (re-matches #"[a-z]" (str character))
    (inc (- (int character) (int \a)))
    (= \S character) 1
    (= \E character) 26))

(def heightmap
  (mapv #(mapv char->elevation %) lines))

(defn get-adjacent-inds [[row col]]
  (let [inds [[row (inc col)] [row (dec col)] [(dec row) col] [(inc row) col]]]
    (filterv #(and (<= 0 (first %) (dec nrow))
                   (<= 0 (second %) (dec ncol)))
             inds)))

(defn get-legal-move-inds [ind]
  (let [adjacent-inds (get-adjacent-inds ind)]
    (filterv #(<= (get-in heightmap %) (inc (get-in heightmap ind)))
             adjacent-inds)))

(defn get-fewest-steps-map
  "Builds a map from index to fewest steps from start-ind.
  Covers all indices (we don't stop at the ending index)."
  [heightmap start-ind legal-moves-fn]
  (loop [fewest-steps {start-ind 0}
         i 1]
    (let [current-inds
          (->> fewest-steps
               (filter (fn [[k v]] (= (dec i) v)))
               (map first))
          new-legal-moves
          (->> (map legal-moves-fn current-inds)
               (reduce into #{})
               (filter #(not-any? #{%} (keys fewest-steps))))]
      (if (empty? new-legal-moves)
        fewest-steps
        (recur (reduce #(assoc %1 %2 i) fewest-steps new-legal-moves)
               (inc i))))))

(def fewest-steps-map
  (get-fewest-steps-map heightmap start-ind get-legal-move-inds))
(println (fewest-steps-map end-ind))

;; ----------
;; Part 2
;; We take the inverse of the "legal move" rule, and start at end-ind.
;; Then find the closest ground-level index.

(defn get-legal-move-inds-inverse [ind]
  (let [adjacent-inds (get-adjacent-inds ind)]
    (filterv #(>= (get-in heightmap %) (dec (get-in heightmap ind)))
             adjacent-inds)))

(def fewest-steps-map-inverse
  (get-fewest-steps-map heightmap end-ind get-legal-move-inds-inverse))

(def ground-level-inds
  (for [row (range nrow)
        col (range ncol)
        :when (= 1 (get-in heightmap [row col]))]
    [row col]))

(->> fewest-steps-map-inverse
     (filter #(some #{(key %)} ground-level-inds))
     (sort-by val)
     first
     println)
