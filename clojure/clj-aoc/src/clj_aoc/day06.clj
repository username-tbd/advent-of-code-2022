(ns clj-aoc.day06
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def stream (first (u/load-lines 6)))

;; -----------
;; First solution (~40ms)
(defn find-marker [stream marker-size]
  (loop [stream stream
         ind marker-size]
    (if (= marker-size (count (distinct (take marker-size stream))))
      ind
      (recur (rest stream) (inc ind)))))

(time [(find-marker stream 4) (find-marker stream 14)])

;; -----------
;; Second solution (~20ms)

(defn find-both-markers [stream m1 m2]
  (loop [stream stream
         found1 false ind1 m1 ind2 m2]
    (if-not found1
      (if (= m1 (count (distinct (take m1 stream))))
        (recur (rest stream) true ind1 (inc ind2))
        (recur (rest stream) false (inc ind1) (inc ind2)))
      (if (= m2 (count (distinct (take m2 stream))))
        [ind1 ind2]
        (recur (rest stream) true ind1 (inc ind2))))))

(time (find-both-markers stream 4 14))

;; -----------
;; Third solution (~2ms)

(defn slide-size [stream marker-size marker-ind]
  (let [leftmost-ind (inc (- marker-ind marker-size))]
    (loop [ind marker-ind char-set #{}]
      (cond
        (char-set (nth stream ind)) (- marker-size (- marker-ind ind))
        (= ind leftmost-ind) nil
        :else (recur (dec ind) (conj char-set (nth stream ind)))))))

(defn find-marker-alt [stream marker-size marker-start]
  (loop [marker-ind marker-start]
    (if-some [slide (slide-size stream marker-size marker-ind)]
      (recur (+ marker-ind slide))
      (inc marker-ind))))

(time
  (let [marker-4 (find-marker-alt stream 4 3)
        start-14 (+ marker-4 (- 14 4))]
   [marker-4 (find-marker-alt stream 14 start-14)]))
