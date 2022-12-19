(ns clj-aoc.day18
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

;; Fix 2 dims (=> axis-orthogonal lines) & only compare within such "ortholines"

(def points
  (->> (u/load-lines 18)
       (mapv (comp #(mapv read-string %)
                   #(clojure.string/split %  #",")))))

(defn point->ortholine-position [point dim]
  (let [ortholine (assoc point dim nil) ; Which ortholine are we on...
        position (get point dim)] ; ... and what is our position within it?
  [ortholine [position]]))

(defn coords->ortholines [point]
  (let [dims (range (count point))]
  (into {} (mapv (partial point->ortholine-position point) dims))))

(def ortholines
  (apply (partial merge-with into) (mapv coords->ortholines points)))

(defn count-adjacencies [coll]
  (let [sorted (sort coll)
        diffs (map - (rest sorted) sorted)]
    (count (filter (partial = 1) diffs))))

(def n-adjacencies 
  (apply + (map count-adjacencies (vals ortholines))))

(def total-cube-sides (* (count points) 6))

(pp (- total-cube-sides (* n-adjacencies 2)))
