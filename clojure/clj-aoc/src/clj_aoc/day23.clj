(ns clj-aoc.day23
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

(def lines (vec (u/load-lines 23)))
(def grove-init (mapv #(mapv {\. :empty \# :elf} %) lines))
(def cardinals-init [:n :s :w :e])

(def direction->steps
  {:n [-1 0] :s [1 0] :w [0 -1] :e [0 1]
   :ne [-1 1] :nw [-1 -1] :se [1 1] :sw [1 -1]})

(def cardinal->directions
  {:n #{:nw :n :ne}
   :s #{:sw :s :se}
   :w #{:sw :w :nw}
   :e #{:se :e :ne}})

(defn cells-in-cardinal [grove elf-index direction]
  (let [adjacent-inds
        (mapv #(mapv + elf-index %) (relative-adjacencies direction))]
    (mapv #(get-in grove %) adjacent-inds)))

(defn get-adjacent-cells [grove elf-index]
  (let [adjacent-inds
        (update-vals direction->steps #(mapv + % elf-index))]
    (update-vals adjacent-inds #(get-in grove %))))

(defn isolated? [adjacent-cells]
  (not-any? #(= :elf %) (vals adjacent-cells)))

(defn should-propose? [adjacent-cells cardinal]
  (let [directions (cardinal->directions cardinal)
        relevant-cells (filter #(contains? directions (key %)) adjacent-cells)]
    (not-any? #(= :elf %) (vals relevant-cells))))

