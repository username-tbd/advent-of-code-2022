(ns clj-aoc.day14
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def sand-source-coord [500 0])

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

(def lowest-y (apply max (map second rock-coords)))

(def cave
  (-> (reduce #(assoc %1 %2 :rock) {} rock-coords)
      (assoc sand-source-coord :sand-source)))

;; ----------
;; Part one

(defn into-the-void [coord]
  (> (second coord) lowest-y))

(defn simulate-unit-falling [cave]
  (loop [[x y] sand-source-coord]
    (let [down [x (inc y)]
          down-left [(dec x) (inc y)]
          down-right [(inc x) (inc y)]]
      (cond
        (nil? (cave down)) 
        (if-not (into-the-void down) (recur down)) ; Else nil!
        (nil? (cave down-left)) 
        (if-not (into-the-void down-left) (recur down-left))
        (nil? (cave down-right)) 
        (if-not (into-the-void down-right) (recur down-right))
        :else
        (assoc cave [x y] :sand)))))

(defn pour-sand [cave]
  (loop [cave cave]
    (if-some [new-cave (simulate-unit-falling cave)]
      (recur new-cave)
      cave)))

(->> (pour-sand cave)
     (filter #(= :sand (val %)))
     count
     println)
