(ns clj-aoc.day01-misc
  (:require [clj-aoc.day01 :as d1])
  (:gen-class))

;; -----------
;; Getting top 3: one pass vs. sort

(defn top-n [n coll]
  (loop [top (sort (take n coll))
         candidates (drop n coll)]
    (if (empty? candidates)
      top
      (recur
        (if (> (first candidates) (first top))
          (sort (cons (first candidates) (rest top)))
          top)
        (rest candidates)))))

;; Benchmark it
(defn noisify [n]
  (if (some? n)
    (->> (rand-int 10000)
        (- 5000)
        (+ n))))

(def big-random-vec
  (->> (reduce into [] (repeat 1000 d1/elf-cal-totals))
       (mapv noisify)))

(println "Length of big-random-vec:")
(println (count big-random-vec)) ; 257000
(println "Sort then take 3:")
(time (->> big-random-vec sort (take-last 3))) ; ~150ms
(println "top-n:")
(time (top-n 3 big-random-vec)) ; ~15ms

;; -----------
;; Variations

(defn reduce-fn-alt
  "Differs in using if-let, and using (conj (pop coll) (do-stuff (last coll)))"
  [coll item]
  (if-let [calories item]
    (conj (pop coll) (+ calories (last coll)))
    (conj coll 0)))
