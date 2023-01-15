(ns clj-aoc.day22
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

(def lines (vec (u/load-lines 22)))
(def map-lines (subvec lines 0 200))
(def path-string (lines 201))

(defn parse-path-string [path-string]
  (let [move-str-pairs 
        (map rest (re-seq #"(\d+)(L|R)" path-string))
        move-pairs
        (map #(vector (Integer. (first %))
                      (keyword (second %)))
             move-str-pairs)]
    (map #(zipmap [:steps :turn] %) move-pairs)))




