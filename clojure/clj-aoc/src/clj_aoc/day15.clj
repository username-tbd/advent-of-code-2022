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

(defn remaining-steps [row]
  (let [dists-to-row
        (mapv #(abs (- (:y %) row)) sensors)
        remaining-steps-at-row
        (mapv - closest-beacon-dists dists-to-row)]
    (->> (zipmap (map (juxt :x :y) sensors) remaining-steps-at-row)
         (remove (comp neg? val))
         (into {}))))

(defn eliminate-ranges [remaining-steps-map]
  (mapv (comp vec (juxt - +))
        (mapv first (keys remaining-steps-map))
        (vals remaining-steps-map)))

(defn find-beacons-on-row [row]
  (->> beacons
       (filter #(= row (:y %)))
       (mapv :x)
       set))

(defn rule-out-points [row]
  (let [remaining-steps-map (remaining-steps row)
        eliminated-ranges (eliminate-ranges remaining-steps-map)
        [left-endpoint right-endpoint] (apply (juxt min max)
                                              (flatten eliminated-ranges))
        beacons-on-row (find-beacons-on-row row)]
    (for [x (range left-endpoint (inc right-endpoint))
          :when (and (some #(<= (first %) x (second %)) eliminated-ranges)
                     (not-any? #{x} beacons-on-row))]
      x)))

(def ruled-out (rule-out-points special-row))
(println (count ruled-out))

;; ----------
;; Part two

(def left-bound 0)
(def right-bound 4000000)

(defn censor-range [r]
  [(max (first r) left-bound) (min (second r) right-bound)])

(defn find-gap [ranges]
  (let [ranges (sort-by first ranges)
        initial-rightmost (second (first ranges))]
    (loop [ranges (rest ranges)
           rightmost initial-rightmost]
      (let [r (first ranges)]
        (cond 
          (nil? r)
          nil
          (> (first r) (inc rightmost))
          (inc rightmost)
          :else
          (recur (rest ranges) (max (second r) rightmost)))))))

(defn find-possible-position [row]
  (let [remaining-steps-map (remaining-steps row)
        eliminated-ranges (eliminate-ranges remaining-steps-map)
        censored-ranges (mapv censor-range eliminated-ranges)]
    (cond
          (pos? (apply min (flatten censored-ranges))) 0 
          (< right-bound (apply max (flatten censored-ranges))) right-bound
          :else (find-gap censored-ranges))))

(time
  (def distress-beacon
    (first
      (for [row (range left-bound (inc right-bound))
            :let [beacon-pos (find-possible-position row)]
            :when (some? beacon-pos)]
        {:x beacon-pos :y row})))) ; => ~160 seconds
  
(println (+ (* (:x distress-beacon) 4000000)
            (:y distress-beacon)))
