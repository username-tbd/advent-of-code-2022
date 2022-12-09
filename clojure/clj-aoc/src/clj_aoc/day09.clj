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

(def unit-moves ; [[1 0] [0 1] [0 1] [-1 0] etc]
  (->> (u/load-lines 9)
       (map move-line->unit-moves-vec) ; Triply nested vectors now
       (reduce concat []))) ; Semi-flattening to a vector of vectors

(defn calc-follower-move [vector-between]
  (if (some #(> (abs %) 1) vector-between)
    (mapv #(compare % 0) vector-between)
    [0 0]))

(defn perform-move [head-pos tail-pos head-move]
  (let [head-pos-new (mapv + head-pos head-move)
        vector-between (mapv - head-pos-new tail-pos)
        tail-move (calc-follower-move vector-between)
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
               (rest head-moves) (conj tail-hits tail-pos-new))))))

(println (count (find-tail-hits unit-moves)))

;; -----------
;; Part Two

(defn perform-move-snake [positions head-move]
  (loop [old-positions positions
         new-positions []
         move head-move]
    (let [old-position (first old-positions)
          new-position (mapv + old-position move)]
      (if (empty? (rest old-positions))
        (conj new-positions (mapv + old-position move))
        (recur (rest old-positions)
               (conj new-positions new-position)
               (calc-follower-move
                 (mapv - new-position (second old-positions))))))))

(defn find-tail-hits-snake [head-moves]
  (loop [positions (vec (repeat 10 [0 0]))
         head-moves head-moves
         tail-hits #{[0 0]}]
    (if (empty? head-moves)
      tail-hits
      (let [positions-new
            (perform-move-snake positions (first head-moves))]
        (recur positions-new
               (rest head-moves) (conj tail-hits (last positions-new)))))))

(println (count (find-tail-hits-snake unit-moves)))
