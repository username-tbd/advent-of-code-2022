(ns clj-aoc.day17
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

;; cycle is very cool. Used twice here.

(def jets-initial
  (->> (first (u/load-lines 17))
    (map (comp keyword str))
    cycle))

(def n-iters 2022)
(def x-margin 2)
(def y-margin 4)
(def jet-to-shift {:< [-1 0] :> [1 0]})

;; 0th row is floor, all spaces occupied
(def occupancies-initial [(into #{} (range 7))]) 

(defn shift-inds [shift inds]
  (mapv #(mapv + shift %) inds))

(def piece-blueprints
  (map (partial shift-inds [x-margin y-margin])
       [[[0 0] [1 0] [2 0] [3 0]]
        [[1 0] [0 1] [1 1] [2 1] [1 2]]
        [[0 0] [1 0] [2 0] [2 1] [2 2]]
        [[0 0] [0 1] [0 2] [0 3]]
        [[0 0] [0 1] [1 0] [1 1]]]))

(def piece-blueprints-cycled
  (take n-iters (cycle piece-blueprints)))

;; get returns nil if key is not found, and (not-any? #{x} nil) returns true!
(defn free? [occupancies [x y]]
  (and (<= 0 x 6) ;; Not hitting a wall
       (< 0 y)
       (not-any? #{x} (get occupancies y))))

(defn jet-piece [piece-inds occupancies jet]
  (let [piece-post-jet (shift-inds (jet-to-shift jet) piece-inds)]
    (if (every? (partial free? occupancies) piece-post-jet)
      piece-post-jet
      piece-inds)))

;; Return nil if the piece cannot move any more
(defn drop-piece [piece-inds occupancies]
  (let [piece-post-drop (shift-inds [0 -1] piece-inds)]
    (if (every? (partial free? occupancies) piece-post-drop)
      piece-post-drop
      nil)))

;; (update [0 10 20] 3 f) => [0 10 20 (f nil)]
;; (clojure.set/union nil #{1}) => #{1}
(defn update-occupancies [occupancies piece]
  (reduce #(update %1 (second %2) clojure.set/union #{(first %2)})
          occupancies piece))

;; Returns map of updated occupancies vector and jets
(defn process-piece [{:keys [occupancies jets]} piece-blueprint]
  (let [highest-row (dec (count occupancies))
        piece-init (shift-inds [0 highest-row] piece-blueprint)]
    (loop [piece-inds piece-init
           jets jets] 
      (let [piece-jetted
            (jet-piece piece-inds occupancies (first jets))]
        (if-some [piece-dropped (drop-piece piece-jetted occupancies)]
          (recur piece-dropped (rest jets))
          {:occupancies (update-occupancies occupancies piece-jetted)
           :jets (rest jets)})))))

(def initial-state {:occupancies occupancies-initial
                    :jets jets-initial})

(def final-state
  (reduce process-piece initial-state piece-blueprints-cycled))

(pp (dec (count (:occupancies final-state))))
