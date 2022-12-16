(ns clj-aoc.day16
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

(def starting-valve-id :AA)

(defn map-valve [line]
  (let [valves (mapv keyword (re-seq #"[A-Z][A-Z]" line))
        flow-rate (read-string (re-find #"\d+" line))]
    [(first valves)
     {:connections (set (rest valves))
      :flow-rate flow-rate}]))

(defn build-valves-map [lines]
  (let [valves (mapv map-valve lines)]
    (into {} valves)))

(def valves (build-valves-map (u/load-lines 16)))

;; -----

(defn keys-with-value [m value]
  (keys (filter #(= (val %) value ) m)))

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
;; Like valves, but we remove all non-relevant (productive or starting) valves
;; and we add, to each relevant valve, a map of distances
;; to every other relevant valve.
(defn prune-distances [distances]
  (apply (partial dissoc distances) irrelevant-valve-ids))

(def graph
  (let [relevant-valves
        (->> valves
             (filter #(some #{(key %)} relevant-valve-ids))
             (into {}))
        excessive-graph ; Contains distances to irrelevant points
        (reduce #(assoc-in %1 [%2 :distances] (build-distances %2))
                relevant-valves relevant-valve-ids)]
    (reduce #(update-in %1 [%2 :distances] prune-distances)
            excessive-graph relevant-valve-ids)))
