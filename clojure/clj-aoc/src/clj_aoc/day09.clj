(ns clj-aoc.day09
  (:require [clj-aoc.util :as u])
  (:gen-class))

(defn move-line->unit-moves-vec
  "'D 3' => [[0 -1] [0 -1] [0 -1]]"
  [move-line]
  (let [[direction magnitude-str] (clojure.string/split move-line #" ")
        magnitude (read-string magnitude-str)
        base-vec ({"R" [1 0] "L" [-1 0] "U" [0 1] "D" [0 -1]} direction)]
    (vec (repeat magnitude base-vec))))

(def unit-moves ; ([1 0] [0 1] [0 1] [-1 0] etc)
  (->> (u/load-lines 9)
       (map move-line->unit-moves-vec) 
       (reduce concat)))

(defn calc-follower-move [leader-pos follower-pos]
  (let [vector-between (mapv - leader-pos follower-pos)]
    (if (some #(> (abs %) 1) vector-between)
      (mapv #(compare % 0) vector-between)
      [0 0])))

(defn perform-move [positions head-move]
  (loop [old-positions positions
         new-positions []
         move head-move]
    (let [leader-pos-old (first old-positions)
          leader-pos-new (mapv + leader-pos-old move)
          follower-pos-old (second old-positions)]
      (if (empty? (rest old-positions))
        (conj new-positions leader-pos-new)
        (recur (rest old-positions)
               (conj new-positions leader-pos-new)
               (calc-follower-move leader-pos-new follower-pos-old))))))

(defn find-tail-hits [head-moves initial-positions]
  (loop [positions initial-positions
         head-moves head-moves
         tail-hits #{[0 0]}]
    (if (empty? head-moves)
      tail-hits
      (let [positions-new
            (perform-move positions (first head-moves))]
        (recur positions-new
               (rest head-moves) (conj tail-hits (peek positions-new)))))))

(defn gen-initial-positions [n-knots]
  (vec (repeat n-knots [0 0])))

(println (count (find-tail-hits unit-moves (gen-initial-positions 2))))
(println (count (find-tail-hits unit-moves (gen-initial-positions 10))))
