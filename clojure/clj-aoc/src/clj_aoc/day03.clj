(ns clj-aoc.day03
  (:require
   [clj-aoc.util :as u])
  (:gen-class))

(def lines
  (with-open [rdr (clojure.java.io/reader "../inputs/input-03.txt")]
    (doall (line-seq rdr))))

(defn split-rucksack [rucksack]
  (split-at (/ (count rucksack) 2) rucksack))

(defn find-shared-type [compartments]
  (some (set (first compartments)) (last compartments)))

(defn type->priority [character]
  (if (re-matches #"[a-z]" (str character))
    (inc (- (int character) (int \a)))
    (+ 27  (- (int character) (int \A)))))

(->> lines
     (map (comp type->priority find-shared-type split-rucksack))
     (apply +)
     println)

;; -----------
;; Part Two

(defn split-groups [lines]
  (for [sack-inds (partition 3 (range (count lines)))]
    (map #(set (nth lines %)) sack-inds)))

(defn find-badge [group]
  (let [in-all-sacks?
        (fn [c] (every? #(some #{c} %) group))]
    (first (filter in-all-sacks? (first group)))))

(->> (split-groups lines)
     (map (comp type->priority find-badge))
     (apply +)
     println)
