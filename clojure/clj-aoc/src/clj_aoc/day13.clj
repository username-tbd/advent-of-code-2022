(ns clj-aoc.day13
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def lines (u/load-lines 13))

(defn read-str-line [line]
  (eval (load-string line)))

(def pairs
  (->> lines (filter not-empty) (map read-str-line) (partition 2)))

;; Scheme-like primitives
(def car first)
(def cdr (comp vec rest))
(def cns (comp vec cons))
(defn null? [x] (if (coll? x) (empty? x) (nil? x)))

;; return whether in correct order
(defn schemer [l1 l2]
  (cond
    (and (null? l1) (null? l2))
      nil
    (or (null? l1) (null? l2))
      (null? l1)
    (and (number? (car l1)) (number? (car l2)))
      (if (= (car l1) (car l2))
        (schemer (cdr l1) (cdr l2))
        (< (car l1) (car l2)))
    (number? (car l1))
      (schemer (cns [(car l1)] (cdr l1)) l2)
    (number? (car l2))
      (schemer l1 (cns [(car l2)] (cdr l2)))
    :else
      (if (null? (schemer (car l1) (car l2)))
        (schemer (cdr l1) (cdr l2))
        (schemer (car l1) (car l2)))))

(->> (map #(apply schemer %) pairs)
     (map-indexed #(if %2 (inc %1) nil)) ; Better way to get inds that are true?
     (filter some?)
     (apply +)
     println)
