(ns clj-aoc.day05-misc
  (:require [clj-aoc.day05 :as d5])
  (:gen-class))

;; -----------
;; Alternative solutions

;; Better to turn it into the vector at the end?
(defn get-stack-vec-alt [crate-lines stack-num]
  (let [col-num (- ( * 4 stack-num) 3)]
    (->> crate-lines
         (map #(get % col-num))
         (filter #(not= \space %))
         reverse
         (into []))))
