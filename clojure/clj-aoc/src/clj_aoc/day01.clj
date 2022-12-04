(ns clj-aoc.day01
  (:require [clj-aoc.util :as u])
  (:gen-class))

;; Load data and convert: ("5" "" "13") => (5 nil 13)
(def lines
  (map #(if (seq %) (Integer/parseInt %))
       (u/load-lines "../../inputs/input-01.txt")))

(def elf-cal-totals
  (-> (fn [coll line-cals]
        (if (some? line-cals)
          (update coll (dec (count coll)) + line-cals) 
          (conj coll 0)))
      (reduce [0] lines)))

(println (apply max elf-cal-totals))
(println (->> elf-cal-totals sort (take-last 3) (apply +)))

;; ----------------------------
;; Alt

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

; Benchmarking
(defn noisify [n]
  (if (some? n)
    (->> (rand-int 10000)
        (- 5000)
        (+ n))))

(def big-random-vec
  (->> (reduce into [] (repeat 1000 elf-cal-totals))
       (mapv noisify)))

(println "Length of big-random-vec:")
(println (count big-random-vec)) ; 257000
(println "Sort then take 3:")
(time (->> big-random-vec sort (take-last 3))) ; ~150ms
(println "top-n:")
(time (top-n 3 big-random-vec)) ; ~15ms

(defn reduce-fn-alt
  "Differs in using if-let, and using (conj (pop coll) (do-stuff (last coll)))"
  [coll item]
  (if-let [calories item]
    (conj (pop coll) (+ calories (last coll)))
    (conj coll 0)))
