(ns clj-aoc.day10
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def lines (u/load-lines 10))

(defn line->instruction-cycles [line]
  (let [[instruction value] (clojure.string/split line #" ")]
    (condp = instruction
      "noop" [{:instruction :noop}]
      "addx" [{:instruction :addx :add-cycle 0 :add (read-string value)}
              {:instruction :addx :add-cycle 1 :add (read-string value)}])))

(def cycles
  (->> (map line->instruction-cycles lines)
       flatten
       (map-indexed (fn [i cycle-map] (assoc cycle-map :cycle i)))))

(def x-history 
  (loop [cycles cycles
         x-seq [1]]
    (let [cyc (first cycles)]
    (if (nil? cyc)
      x-seq
      (if (= 1 (:add-cycle cyc))
        (recur (rest cycles) (conj x-seq (+ (:add cyc) (peek x-seq))))
        (recur (rest cycles) (conj x-seq (peek x-seq))))))))

(->> (map-indexed (fn [i x] (* (inc i) x)) x-history)
     (drop 19)
     (take-nth 40)
     (apply +)
     (println))


