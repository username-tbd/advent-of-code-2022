(ns clj-aoc.day05
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def lines-split (split-with not-empty (u/load-lines 5)))
(def crate-lines (butlast (first lines-split)))
(def step-lines (rest (second lines-split)))
(def crate-nums (range 1 10))

(defn parse-step-line [line]
  (->> line
       (re-find #"move (\d+) from (\d+) to (\d+)")
       rest
       (map read-string)
       (zipmap [:n :from :to])))
       
(def steps (map parse-step-line step-lines))

(defn get-crate-vec [crate-num]
  (let [col-num (- ( * 4 crate-num) 3)]
    (->> crate-lines
      (map #(get % col-num))
      reverse
      (into [])
      (filterv #(not= \space %)))))

(def crates (->> (map get-crate-vec crate-nums)
                 (zipmap crate-nums)))

(defn move-crate [{:keys [from to]} crates]
  (-> crates
      (update to conj (peek (crates from)))
      (update from pop)))

(defn execute-step [{:keys [from to n]} crates]
  (let [move-correct-crate
        (partial move-crate {:from from :to to})]
    (nth (iterate move-correct-crate crates) n)))

(defn get-final-crates [steps crates step-fn]
  (loop [steps steps
         crates crates]
    (if (empty? steps)
      crates
      (recur (rest steps)
             (step-fn (first steps) crates)))))

(defn get-answer-string [final-crates]
  (->> crate-nums
       (map (comp last final-crates))
       (apply str)))

(println (get-answer-string (get-final-crates steps crates execute-step)))

;; -----------
;; Part Two

(defn execute-step-v2 [{:keys [from to n]} crates]
  (-> crates
      (update to #(apply conj % (take-last n (crates from))))
      (update from #(into [] (drop-last n %)))))

(println (get-answer-string (get-final-crates steps crates execute-step-v2)))
