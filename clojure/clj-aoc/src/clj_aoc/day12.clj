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

(def start-ind-pt1 (find-char-ind \S))
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

(defn get-fewest-steps 
  "Builds a map from index to fewest steps from start-ind.
  Covers all indices (we don't stop at the ending index)."
  [heightmap start-ind]
  (loop [fewest-steps {start-ind 0}
         i 1]
    (let [current-inds
          (->> fewest-steps
               (filter (fn [[k v]] (= (dec i) v)))
               (map first))
          new-legal-moves
          (->> (map get-legal-move-inds current-inds)
               (reduce into #{})
               (filter #(not-any? #{%} (keys fewest-steps))))]
      (if (empty? new-legal-moves)
        fewest-steps
        (recur (reduce #(assoc %1 %2 i) fewest-steps new-legal-moves)
               (inc i))))))

(def fewest-steps (get-fewest-steps heightmap start-ind-pt1))
(println (fewest-steps end-ind))
