(ns clj-aoc.day05-misc
  (:require [clj-aoc.day05 :as d5])
  (:gen-class))

;; -----------
;; Alternative solutions

;; Better to turn it into the vector at the end?
(defn get-crate-vec-alt [crate-lines crate-num]
  (let [col-num (- ( * 4 crate-num) 3)]
    (->> crate-lines
         (map #(get % col-num))
         (filter #(not= \space %))
         reverse
         (into []))))
