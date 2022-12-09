(ns clj-aoc.day09
  (:require [clj-aoc.util :as u])
  (:gen-class))

(defn move-seq->unit-moves-vec
  "('D' '3') => [[0 -1] [0 -1] [0 -1]]"
  [move-seq]
  (let [direction (first move-seq)
        magnitude (read-string (second move-seq))
        base-vec ({"R" [1 0] "L" [-1 0] "U" [0 1] "D" [0 -1]} direction)]
    (vec (repeat magnitude base-vec))))

(def parse-line ; Is this weird?
    (comp move-seq->unit-moves-vec rest (partial re-find #"([RLUD]) (\d+)")))

(def unit-moves ; [[1 0] [0 1] [0 1] [-1 0] etc]
  (->> (u/load-lines 9)
       (map parse-line)
       (reduce concat []))) ; Semi-flattening

(defn calc-tail-move [vector-between]
  (if (some #(> (abs %) 1) vector-between)
    (mapv #(compare % 0) vector-between)
    [0 0]))

(defn perform-move [head-pos tail-pos head-move]
  (let [head-pos-new (mapv + head-pos head-move)
        vector-between (mapv - head-pos-new tail-pos)
        tail-move (calc-tail-move vector-between)
        tail-pos-new (mapv + tail-pos tail-move)]
    [head-pos-new tail-pos-new]))

(defn find-tail-hits [head-moves]
  (loop [head-pos [0 0]
         tail-pos [0 0]
         head-moves head-moves
         tail-hits #{[0 0]}]
    (if (empty? head-moves)
      tail-hits
      (let [[head-pos-new tail-pos-new]
            (perform-move head-pos tail-pos (first head-moves))]
        (recur head-pos-new tail-pos-new
               (next head-moves) (conj tail-hits tail-pos-new))))))

(println (count (find-tail-hits unit-moves)))

