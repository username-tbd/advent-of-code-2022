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
       (map-indexed (fn [i cycle-map] (assoc cycle-map :cycle-num (inc i))))))

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

;; Part two
;; ----------------

(def crt-pixels 200)
(def crt-row-len 40)

(def cycles-with-x
  (map-indexed
    (fn [i m] (assoc m :x (x-history i)))
    cycles))

(defn get-cycle-column [cycle-num]
    (rem (dec cycle-num) crt-row-len))

(defn get-pixel [cyc]
  (let [cycle-column
        (get-cycle-column (:cycle-num cyc))
        sprite-columns
        ((juxt dec identity inc) (:x cyc))]
    (if (some #{cycle-column} sprite-columns) "#" ".")))

(def pixels-flat
  (loop [cycles cycles-with-x
         pixels []]
    (let [cyc (first cycles)]
      (if (nil? cyc)
        pixels
        (recur (rest cycles)
               (conj pixels (get-pixel cyc)))))))

(def screen
  (->> (mapv #(subvec pixels-flat % (+ % crt-row-len))
             (range 0 (inc crt-pixels) crt-row-len))
       (mapv #(apply str %))))

(map println screen)
