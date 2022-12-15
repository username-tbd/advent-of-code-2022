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


