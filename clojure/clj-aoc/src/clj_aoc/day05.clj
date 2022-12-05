(ns clj-aoc.day05
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def lines-split (split-with not-empty (u/load-lines 5)))
(def crate-lines (butlast (first lines-split)))
(def step-lines (rest (second lines-split)))
(def stack-nums (range 1 10))
(def max-stack-height 8)

(defn get-stack-vec [crate-lines stack-num]
  (let [col-num (- ( * 4 stack-num) 3)
        row-nums (reverse (range max-stack-height))
        char-vec (mapv #(get-in (vec crate-lines) [% col-num]) row-nums)]
    (filterv #(not= \space %) char-vec)))

(defn parse-step-line [line]
  (->> line
       (re-find #"move (\d+) from (\d+) to (\d+)")
       rest
       (map read-string)
       (zipmap [:n :from :to])))
       
(def stacks (zipmap stack-nums (map #(get-stack-vec crate-lines %) stack-nums)))
(def steps (map parse-step-line step-lines))

(defn move-crate [{:keys [from to]} stacks]
  (-> stacks
      (update to conj (peek (stacks from)))
      (update from pop)))

(defn execute-step [{:keys [from to n]} stacks]
  (let [move-correct-crate
        (partial move-crate {:from from :to to})]
    (nth (iterate move-correct-crate stacks) n)))

(defn get-final-stacks [steps stacks step-fn]
  (loop [steps steps
         stacks stacks]
    (if (empty? steps)
      stacks
      (recur (rest steps) (step-fn (first steps) stacks)))))

(defn get-answer-string [final-stacks]
  (->> stack-nums
       (map (comp last final-stacks))
       (apply str)))

(println (get-answer-string (get-final-stacks steps stacks execute-step)))

;; -----------
;; Part Two

(defn execute-step-v2 [{:keys [from to n]} stacks]
  (-> stacks
      (update to #(apply conj % (take-last n (stacks from))))
      (update from #(into [] (drop-last n %)))))

(println (get-answer-string (get-final-stacks steps stacks execute-step-v2)))
