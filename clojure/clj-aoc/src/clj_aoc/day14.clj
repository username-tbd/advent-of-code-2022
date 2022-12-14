(ns clj-aoc.day14
  (:require [clj-aoc.util :as u])
  (:gen-class))

;; Lots of parsing here. Get the [x,y] coordinate of every individual rock,
;; then convert these coords to [row,col] indices out of which we make a matrix.

(defn text-line->coord-vec [line]
  (->> (clojure.string/split line #" -> ")
       (mapv #(clojure.string/split % #","))
       (mapv #(mapv read-string %))))

(defn min-max [coll]
  ((juxt (partial apply min) (partial apply max)) coll))

;; Assumes one of either x or y will be constant!
(defn fill-out-coord-pair [pair]
  (let [[x-min x-max] (min-max (map first pair))
        [y-min y-max] (min-max (map second pair))]
    (for [x (range x-min (inc x-max))
          y (range y-min (inc y-max))]
      [x y])))

(defn coord-vec->rocks [coord-vec]
  (let [coord-pairs (partition 2 1 coord-vec)]
    (->> (mapv fill-out-coord-pair coord-pairs)
         (reduce into [])
         distinct))) ; There are "backtracks" in the input data

(def rock-coords
  (->> (u/load-lines 14)
       (mapv (comp coord-vec->rocks text-line->coord-vec))
       (reduce into [])
       distinct))

(def sand-source-coord [500 0])
(def all-coords (conj rock-coords sand-source-coord))

;; Now create the matrix that represents our cave.
(def min-x (apply min (map first all-coords)))
(def min-y (apply min (map second all-coords)))

(defn coord->ind [[x y]]
  [(- y min-y) (- x min-x)])

(def rock-inds (mapv coord->ind rock-coords))
(def sand-source-ind (coord->ind sand-source-coord))
(def all-inds (conj rock-inds sand-source-ind))

; Now we build the actual cave of air and rock
(def cave
  (let [n-rows (inc (apply max (map first rock-inds)))
        n-cols (inc (apply max (map second rock-inds)))
        repeatv (comp vec repeat)
        empty-cave (repeatv n-rows (repeatv n-cols :air))]
    (reduce #(assoc-in %1 %2 :rock) empty-cave rock-inds)))

;; ----------
;; Part one

(defn simulate-unit-falling [cave]
  (loop [[row col] sand-source-ind]
    (let [down (get-in cave [(inc row) col])
          down-left (get-in cave [(inc row) (dec col)])
          down-right (get-in cave [(inc row) (inc col)])]
      (cond
        (= :air down) (recur [(inc row) col])
        (nil? down) nil ; Out-of-bounds, endless void
        (= :air down-left) (recur [(inc row) (dec col)])
        (nil? down-left) nil
        (= :air down-right) (recur [(inc row) (inc col)])
        (nil? down-right) nil
        :else (assoc-in cave [row col] :sand)))))

(defn pour-sand [cave]
  (loop [cave cave]
    (if-some [new-cave (simulate-unit-falling cave)]
      (recur new-cave)
      cave)))

(->> (pour-sand cave)
     flatten
     (filter #(= :sand %))
     count)
