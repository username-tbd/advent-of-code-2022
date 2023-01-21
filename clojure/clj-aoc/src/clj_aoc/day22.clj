(ns clj-aoc.day22
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

;; ----
;; Part 2: Going to manually define one of the wrap scenarios for every edge,
;; and generate their respective complements with reverse-cube-wrap.

(def flip-direction {:up :down :down :up :left :right :right :left})

(defn reverse-cube-wrap-pair [wrap-pair]
  (let [new-key (val wrap-pair)
        new-val (key wrap-pair)]
    {(update new-key :direction flip-direction)
     (update new-val :direction flip-direction)}))

(def a-wraps
  (zipmap 
    (mapv (fn [pos] {:direction :down :position (vector 49 pos)})
          (range 100 150))
    (mapv (fn [pos] {:direction :left :position (vector pos 99)})
          (range 50 100))))

(def b-wraps
  (zipmap 
    (mapv (fn [pos] {:direction :up :position (vector 100 pos)})
          (range 0 50))
    (mapv (fn [pos] {:direction :right :position (vector pos 50)})
          (range 50 100))))

(def c-wraps
  (zipmap 
    (mapv (fn [pos] {:direction :down :position (vector 149 pos)})
          (range 50 100))
    (mapv (fn [pos] {:direction :left :position (vector pos 49)})
          (range 150 200))))

(def d-wraps
  (zipmap 
    (mapv (fn [pos] {:direction :up :position (vector 0 pos)})
          (range 50 100))
    (mapv (fn [pos] {:direction :right :position (vector pos 0)})
          (range 150 200))))

(def e-wraps
  (zipmap 
    (mapv (fn [pos] {:direction :left :position (vector pos 50)})
          (range 0 50))
    (mapv (fn [pos] {:direction :right :position (vector pos 0)})
          (reverse (range 100 150)))))

(def f-wraps
  (zipmap 
    (mapv (fn [pos] {:direction :up :position (vector 0 pos)})
          (range 100 150))
    (mapv (fn [pos] {:direction :up :position (vector 199 pos)})
          (range 0 50))))

(def g-wraps
  (zipmap 
    (mapv (fn [pos] {:direction :right :position (vector pos 99)})
          (range 100 150))
    (mapv (fn [pos] {:direction :left :position (vector pos 149)})
          (reverse (range 0 50)))))

(def wraps
  (let [wraps-one-way 
        (merge a-wraps b-wraps c-wraps d-wraps e-wraps f-wraps g-wraps)
        wraps-reversed
        (into {} (map reverse-cube-wrap-pair wraps-one-way))]
    (merge wraps-one-way wraps-reversed)))

;; -----------------------------

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


;; "Walk" back until we hit the first null on the other side.
(defn wrap-pt1 [board position direction]
  (let [walkback-dir (flip-direction direction)]
    (loop [position position]
      (let [next-position (get-next-index-raw position walkback-dir)]
        (if (nil? (get-in board next-position))
          [position direction]
          (recur next-position))))))

;; Cube wrapping
(defn wrap-pt2 [position direction]
  (let [new-status
        (wraps {:direction direction :position position})]
    [(:position new-status) (:direction new-status)]))

(defn wrap [board position direction part]
  (if (= part 1)
    (wrap-pt1 board position direction)
    (wrap-pt2 position direction)))
  
(defn get-next-status [board [row col] direction part]
  (let [next-index-raw
        (get-next-index-raw [row col] direction)]
    (if (some? (get-in board next-index-raw))
      [next-index-raw direction]
      (wrap board [row col] direction part)))) 

;; Returns new position (and, as of part 2, direction)
(defn walk-path-element
  [board position direction n-steps part]
  (loop [n-steps n-steps
         position position
         direction direction]
    (let [[next-index next-direction]
          (get-next-status board position direction part)]
      (cond
        (zero? n-steps)
        {:position position :direction direction}
        (= (get-in board next-index) :wall)
        {:position position :direction direction}
        :else
        (recur (dec n-steps) next-index next-direction)))))

(defn rotate [direction turn]
  (cond 
    (some #{[direction turn]} #{[:right :L] [:left :R]}) :up
    (some #{[direction turn]} #{[:right :R] [:left :L]}) :down
    (some #{[direction turn]} #{[:up :L] [:down :R]}) :left
    (some #{[direction turn]} #{[:up :R] [:down :L]}) :right
    ;; See comment above parse-path-string
    :else direction))

(defn get-final-status
  [board initial-position initial-direction path part]
  (loop [position initial-position
         direction initial-direction
         path path]
    (if (empty? path)
      {:position position :direction direction}
      (let [post-walk-status 
            (walk-path-element board position direction
                                (:steps (first path)) part)]
      (recur (:position post-walk-status)
             (rotate (:direction post-walk-status) (:turn (first path)))
             (rest path))))))

(def final-status (get-final-status board [0 50] :right path 1))

(pp (+ (* 1000 (inc (first (:position final-status))))
       (* 4 (inc (second (:position final-status))))
       ((:direction final-status) {:right 0 :down 1 :left 2 :up 3})))

;; ------- Part two

(def final-status-pt2 (get-final-status board [0 50] :right path 2))

(pp
  (+ (* 1000 (inc (first (:position final-status-pt2))))
     (* 4 (inc (second (:position final-status-pt2))))
     ((:direction final-status-pt2) {:right 0 :down 1 :left 2 :up 3})))

