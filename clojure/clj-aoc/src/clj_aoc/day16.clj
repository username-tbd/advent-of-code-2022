(ns clj-aoc.day16
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

(def starting-valve-id :AA)
(def n-minutes 30)
(def minutes (range 1 (inc n-minutes)))

;; If you open a valve in minute m, what is ultimate total flow?
;; Using a sorted map because minutes are 1-indexed in this problem
;; and I don't want to get confused with (0-indexed) vectors.
(defn build-total-flow-map [flow-rate]
  (let [total-flow
        (mapv #(* (- n-minutes %) flow-rate) minutes)] 
    (into (sorted-map) (zipmap minutes total-flow))))

(defn map-valve [line]
  (let [valves (mapv keyword (re-seq #"[A-Z][A-Z]" line))
        flow-rate (read-string (re-find #"\d+" line))]
    [(first valves)
     {:connections (set (rest valves))
      :flow-rate flow-rate
      :total-flow-map (build-total-flow-map flow-rate)}]))

(defn build-valves-map [lines]
  (let [valves (mapv map-valve lines)]
    (into {} valves)))

(def valves (build-valves-map (u/load-lines 16)))

(defn keys-with-value [m value]
  (keys (filter #(= (val %) value) m)))

(defn build-distances [valve-id]
  (loop [steps-map {valve-id 0}
         n-steps 1]
    (let [current-valves
          (keys-with-value steps-map (dec n-steps))
          connections
          (reduce into #{} (mapv #(get-in valves [% :connections])
                                 current-valves))
          new-valves
          (filter #(not-any? #{%} (keys steps-map))
                  connections)]
      (if (empty? new-valves)
        steps-map
        (recur (reduce #(assoc %1 %2 n-steps) steps-map new-valves)
               (inc n-steps))))))

(def productive-valve-ids (keys (filter (comp pos? :flow-rate val) valves)))
(def relevant-valve-ids (conj productive-valve-ids starting-valve-id))
(def irrelevant-valve-ids (remove #(some #{%} relevant-valve-ids)
                                  (keys valves)))

;; Now we build the main data structure, held in graph.
;; Like valves, but we only retain relevant (productive or starting) valves
;; and we add, to each relevant valve, a map of distances
;; to every other relevant valve.
(defn prune-distances [distances]
  (apply (partial dissoc distances) irrelevant-valve-ids))

(defn remove-own-distances [map-element]
  (let [this-valve (key map-element)
        distances (:distances (val map-element))
        own-removed (dissoc distances this-valve)]
    (assoc-in map-element [1 :distances] own-removed)))

(def graph
  (let [relevant-valves
        (->> valves
             (filter #(some #{(key %)} relevant-valve-ids))
             (into {}))
        excessive-graph ; Contains distances to irrelevant points
        (reduce #(assoc-in %1 [%2 :distances] (build-distances %2))
                relevant-valves relevant-valve-ids)
        relevant-graph
        (reduce #(update-in %1 [%2 :distances] prune-distances)
                excessive-graph relevant-valve-ids)]
    (into {} (map remove-own-distances relevant-graph))))

;; If I'm at valve move-from and want to go to move-to (and
;; immediately open the valve), how many points will that ultimately be?
(defn action->points [minute move-from move-to]
  (let [distance (get-in graph [move-from :distances move-to])
        opening-minute (+ minute distance)]
    (get-in graph [move-to :total-flow-map opening-minute] 0)))

;; might not be needed
; (defn other-productives [valve-id]
;   (remove #{valve-id} productive-valve-ids))

;; delete once generalized
(defn best-option-single [minute move-from closed-valves]
  (apply max-key #(action->points minute move-from %) closed-valves))

; (defn best-option [minute move-from closed-valves n-foresight])

(loop [minute 1
       valve :AA
       points 0
       closed-valves productive-valve-ids]
  (pp minute)
  (if (>= minute 30)
    points
    (let [next-valve (best-option-single minute valve closed-valves)
          new-points (action->points minute valve next-valve)
          distance (get-in graph [valve :distances next-valve])
          minutes-spent (inc distance)]
      (recur (+ minute minutes-spent)
             next-valve
             (+ points new-points)
             (remove #{next-valve} closed-valves)))))




