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

(def elevations
  (mapv #(mapv char->elevation %) lines))

(defn adjacent-inds [[row col]]
  (let [inds [[row (inc col)] [row (dec col)] [(dec row) col] [(inc row) col]]]
    (filterv #(and (<= 0 (first %) (dec nrow))
                   (<= 0 (second %) (dec ncol)))
             inds)))

(defn legal-move-inds [elevations ind]
  (->> (adjacent-inds ind)
       (filterv #(<= (get-in elevations %) (inc (get-in elevations ind))))))

(defn keys-with-value [m value]
  (keys (filter #(= (val %) value ) m)))

(defn fewest-steps
  "Returns a map from elevations index to the shortest path from start-ind.
  e.g., the entry {[30 43] 27} means the shortest path from start-ind to
  [30 43] is 27 steps.
  ALL points in the elevations matrix are swept and thus represented as keys.
  This means that this function does not care about the end point!"
  [elevations start-ind legal-moves-fn]
  (loop [fewest-steps-map {start-ind 0}
         n-steps 1]
    (let [current-inds
          (keys-with-value fewest-steps-map (dec n-steps))
          legal-moves
          (reduce into #{} (map (partial legal-moves-fn elevations)
                                current-inds))
          new-moves
          (filter #(not-any? #{%} (keys fewest-steps-map))
                  legal-moves)]
      (if (empty? new-moves)
        fewest-steps-map
        (recur (reduce #(assoc %1 %2 n-steps) fewest-steps-map new-moves)
               (inc n-steps))))))

(def fewest-steps-map
  (fewest-steps elevations start-ind legal-move-inds))
(println (fewest-steps-map end-ind))

;; ----------
;; Part 2
;; We start from end-ind, adopt the inverse of the rule from part 1 (you can
;; only go down 1 level while going up is not limited), and perform the
;; same process in reverse: spread out from our initial point, collecting
;; each point's minimum distance from it.
;; Then, we find the winner among the points at the lowest elevation.
(defn legal-backtrack-inds [elevations ind]
  (->> (adjacent-inds ind)
       (filterv #(>= (get-in elevations %) (dec (get-in elevations ind))))))

(def fewest-steps-map-inverse
  (fewest-steps elevations end-ind legal-backtrack-inds))

(->> fewest-steps-map-inverse
     (filter #(= 1 (get-in elevations (key %))))
     (map val)
     (apply min)
     println)
