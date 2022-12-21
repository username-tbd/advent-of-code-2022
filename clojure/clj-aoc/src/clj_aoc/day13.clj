(ns clj-aoc.day13
  (:require [clj-aoc.util :as u])
  (:gen-class))

(defn line->packet [line]
  (let [paren-line (clojure.string/replace line #"\[|\]" {"[" "(" "]" ")"})]
    (load-string (str "'" paren-line))))

(def packets
  (->> (u/load-lines 13)
       (filter not-empty)
       (map line->packet)))

(def pairs (partition 2 packets))

;; Scheme-like primitives
(def car first)
(def cdr rest)
(defn null? [x] (if (coll? x) (empty? x) (nil? x)))

;; Return whether l1 and l2 are ordered
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
      (schemer (cons [(car l1)] (cdr l1)) l2)
    (number? (car l2))
      (schemer l1 (cons [(car l2)] (cdr l2)))
    :else
      (if (null? (schemer (car l1) (car l2)))
        (schemer (cdr l1) (cdr l2))
        (schemer (car l1) (car l2)))))

(defn which-inds [coll] ; 1-based
  (keep-indexed #(if %2 (inc %1)) coll))

(->> (map #(apply schemer %) pairs)
     which-inds
     (apply +)
     println)

;; -------------
;; Part two

(def new-packets '(((2)) ((6))))
(def flat-packets (concat packets new-packets))

(defn schemer-comparator [l1 l2]
  (let [l1-smaller (schemer l1 l2)]
    (cond
      l1-smaller -1
      (nil? l1-smaller) 0
      :else 1)))

(->> (sort schemer-comparator flat-packets)
     (map (set new-packets))
     which-inds
     (apply *)
     println)
