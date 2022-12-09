(ns clj-aoc.day09
  (:require [clj-aoc.util :as u])
  (:gen-class))

(defn translate-move [move]
  "'D 5' => [0 -5]"
  (let [direction (first move)
        magnitude (read-string (second move))]
    (condp = direction
      "R" [magnitude 0]
      "L" [(- magnitude) 0]
      "U" [0 magnitude]
      "D" [0 (- magnitude)])))

(def parse-line ; Is this weird?
    (comp translate-move rest (partial re-find #"([RLUD]) (\d+)")))

(def head-moves (map parse-line (u/load-lines 9)))

(defn calc-tail-move [vector-between]
  (mapv #(compare % 0) vector-between))

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

(println (count (find-tail-hits head-moves)))

