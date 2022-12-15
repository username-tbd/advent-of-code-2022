(ns clj-aoc.day15
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def lines (u/load-lines 15))
(def special-row 2000000)

(def pairs ; List of sensor/beacon pairs.
  (mapv (comp (partial partition 2)
              (partial mapv read-string)
              (partial re-seq #"-*\d+"))
        lines))

(def sensors (mapv #(zipmap [:x :y] (first %)) pairs))
(def beacons (mapv #(zipmap [:x :y] (second %)) pairs))

(def closest-beacon-dists
  (mapv #(+ (abs (- (:x %1) (:x %2)))
            (abs (- (:y %1) (:y %2))))
        sensors beacons))

(def special-row-dists
  (mapv #(abs (- (:y %) special-row)) sensors))

;; i.e., remaining steps once a straight line gets you down to the special row
(def remaining-steps 
    (mapv - closest-beacon-dists special-row-dists))

;; Put into a map, filter on ones that can rule at least one point out
(def remaining-steps-map
  (->>
    (zipmap (map (juxt :x :y) sensors) remaining-steps)
    (remove (comp neg? val))
    (into {})))

(def special-row-ranges
  (mapv (comp vec (juxt - +))
        (mapv first (keys remaining-steps-map)) (vals remaining-steps-map)))

(def left-endpoint
  (apply min (mapv first special-row-ranges)))
(def right-endpoint
  (apply max (mapv second special-row-ranges)))

(def special-row-beacon-positions
  (->> beacons
       (filter #(= special-row (:y %)))
       (mapv :x)
       set))

(def ruled-out
  (for [x (range left-endpoint (inc right-endpoint))
        :when (and (some #(<= (first %) x (second %)) special-row-ranges)
                   (not-any? #{x} special-row-beacon-positions))]
    x))
  
(println (count ruled-out))
