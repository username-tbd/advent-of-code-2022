(ns clj-aoc.day05
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def lines-split (split-with not-empty (u/load-lines 5)))
(def crate-lines (butlast (first lines-split)))
(def step-lines (rest (second lines-split)))
(def crate-nums (range 1 10))
(def max-stack-height 8)

(defn get-stack-vec [crate-lines crate-num]
  (let [col-num (- ( * 4 crate-num) 3)
        row-nums (reverse (range max-stack-height))
        char-vec (mapv #(get-in (vec crate-lines) [% col-num]) row-nums)]
    (filterv #(not= \space %) char-vec)))

(defn parse-step-line [line]
  (->> line
       (re-find #"move (\d+) from (\d+) to (\d+)")
       rest
       (map read-string)
       (zipmap [:n :from :to])))
       
(def stacks (zipmap crate-nums (map #(get-stack-vec crate-lines %) crate-nums)))
(def steps (map parse-step-line step-lines))

(defn move-crate [{:keys [from to]} stacks]
  (-> stacks
      (update to conj (peek (stacks from)))
      (update from pop)))

(defn execute-step [{:keys [from to n]} stacks]
  (let [move-correct-crate
        (partial move-crate {:from from :to to})]
    (nth (iterate move-correct-crate stacks) n)))

(defn get-final-crates [steps stacks step-fn]
  (loop [steps steps
         stacks stacks]
    (if (empty? steps)
      stacks
      (recur (rest steps) (step-fn (first steps) stacks)))))

(defn get-answer-string [final-crates]
  (->> crate-nums
       (map (comp last final-crates))
       (apply str)))

(println (get-answer-string (get-final-crates steps stacks execute-step)))

;; -----------
;; Part Two

(defn execute-step-v2 [{:keys [from to n]} stacks]
  (-> stacks
      (update to #(apply conj % (take-last n (stacks from))))
      (update from #(into [] (drop-last n %)))))

(println (get-answer-string (get-final-crates steps stacks execute-step-v2)))
