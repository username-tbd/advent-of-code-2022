(ns clj-aoc.day22
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

(def lines (vec (u/load-lines 22)))
(def board-lines (subvec lines 0 200))
(def path-string (lines 201))

;; I assumed every number was followed by a letter. Wrong.
;; The last number is not. Hence the * in (L|R)*.
(defn parse-path-string [path-string]
  (let [move-str-pairs 
        (map rest (re-seq #"(\d+)(L|R)*" path-string))
        move-pairs
        (map #(vector (Integer. (first %))
                      (keyword (second %)))
             move-str-pairs)]
    (map #(zipmap [:steps :turn] %) move-pairs)))

(defn parse-board-lines [board-lines]
 (let [parse-tile
       ;; nil so it's just like when get-in fails.
       ;; Functionally the same for this problem!
       {\. :open \# :wall \space nil}]
  (mapv #(mapv parse-tile %) board-lines))) 

(def path (parse-path-string path-string))
(def board (parse-board-lines board-lines))

(defn get-next-index-raw [[row col] direction]
  (condp = direction
    :up [(dec row) col]
    :down [(inc row) col]
    :left [row (dec col)]
    :right [row (inc col)]))

(def flip-direction {:up :down :down :up :left :right :right :left})

;; "Walk" back until we hit the first null on the other side.
(defn wrap-index [board position direction]
  (let [walkback-dir (flip-direction direction)]
    (loop [position position]
      (let [next-position (get-next-index-raw position walkback-dir)]
        (if (nil? (get-in board next-position))
          position
          (recur next-position))))))
  
(defn get-next-index [board [row col] direction]
  (let [next-index-raw
        (get-next-index-raw [row col] direction)]
    (if (some? (get-in board next-index-raw))
      next-index-raw
      (wrap-index board [row col] direction)))) 

;; Returns new position
(defn walk-path-element
  [board position direction n-steps]
  (loop [n-steps n-steps
         position position]
    (let [next-index (get-next-index board position direction)]
      (cond
        (zero? n-steps) position
        (= (get-in board next-index) :wall) position
        :else (recur (dec n-steps) next-index)))))

(defn rotate [direction turn]
  (cond 
    (some #{[direction turn]} #{[:right :L] [:left :R]}) :up
    (some #{[direction turn]} #{[:right :R] [:left :L]}) :down
    (some #{[direction turn]} #{[:up :L] [:down :R]}) :left
    (some #{[direction turn]} #{[:up :R] [:down :L]}) :right
    ;; See comment above parse-path-string
    :else direction))

(defn get-final-status
  [board initial-position initial-direction path]
  (loop [position initial-position
         direction initial-direction
         path path]
    (if (empty? path)
      {:position position :direction direction}
      (recur (walk-path-element board position direction
                                (:steps (first path)))
             (rotate direction (:turn (first path)))
             (rest path)))))

(def final-status (get-final-status board [0 50] :right path))

(+ (* 1000 (inc (first (:position final-status))))
   (* 4 (inc (second (:position final-status))))
   ((:direction final-status) {:right 0 :down 1 :left 2 :up 3}))

;; ------- Part two

;; Going to manually define half of the wrap scenarios,
;; and generate the other half from those.


(zipmap 
  (mapv (fn [pos] {:direction :up :position (vector 0 pos)})
        (range 50 100))
  (mapv (fn [pos] {:direction :right :position (vector pos 0)})
        (range 150 200)))
