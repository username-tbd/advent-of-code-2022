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

;; ----------
;; Part two
;; Don't think I can use the aboe - so we'll walk the exterior.
;; Note that the unit-length walk-vecs I use are with respect to "sidling"
;; from one cube to another - on both cube's same side.

;; Ex: I'm on a cube's top and my walk-vec is [1 0 0].
;; That means I'm trying to go 1 unit to the right, i.e.,
;; to the top of the cube directly to my right.
;; However, I might get blocked by a cube face directly to my right
;; (in which case I go to that blocking face).
;; Or there might be no cube to my right at all - in which case I
;; wrap around the cube I'm on, over to its right side.

(def cube-faces ; All cube faces
  (for [side [:left :right :front :back :top :bottom]
        point points]
    {:coord point :side side}))

;; Need a face that we know is external.
;; Pick a top face on a cube that is at the structure's max height.
(def starting-face 
  (let [max-z
        (apply max (map #(last (:coord %)) cube-faces))]
    (first (filter #(and (= (last (:coord %)) max-z)
                         (= (:side %) :top))
                   cube-faces))))

;; Which directions can you choose to walk given your side?
;; Ex: If you are on the top side of a cube, then you can consider moving
;; to adjacent cubes' top faces to the left, right, back, and forward:
;; [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0]]
(def side->walk-vecs
  (let [left-right [[0 0 1] [0 0 -1] [0 1 0] [0 -1 0]]
        top-bottom [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0]]
        front-back [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1]]]
    {:left left-right :right left-right
     :top top-bottom :bottom top-bottom
     :front front-back :back front-back}))

(defn vector-add [v1 v2]
  (mapv + v1 v2))

(def blocker-vec-adders ; A helper, not sure how to name this.
  {:left [-1 0 0] :right [1 0 0]
   :top [0 0 1] :bottom [0 0 -1]
   :front [0 -1 0] :back [0 1 0]})

;; Ex: If I'm trying to walk to the right (walk-vec of [1 0 0]),
;; a blocking face must be on the left side of its cube.
(def walk-vec->blocker-side
  {[1 0 0] :left [-1 0 0] :right
   [0 1 0] :front [0 -1 0] :back
   [0 0 1] :bottom [0 0 -1] :top})

(defn get-potential-blocker-position [cube-face walk-vec]
  (let [vec-to-blocker
        (vector-add walk-vec ((:side cube-face) blocker-vec-adders))]
    (vector-add (:coord cube-face) vec-to-blocker)))

;; Walking "into" a face that is blocking your desired direction.
(defn blocking-walk [cube-face walk-vec]
  (let [blocker-position (get-potential-blocker-position cube-face walk-vec)
        blocker-side (walk-vec->blocker-side walk-vec)]
    (first (filter
             #(= {:coord blocker-position :side blocker-side} %)
             cube-faces))))

;; Walking "along" two flush cubes in your desired direction.
(defn sidle-walk [cube-face walk-vec]
  (let [sidlee-position (vector-add (:coord cube-face) walk-vec)]
    (first (filter
             #(= {:coord sidlee-position :side (:side cube-face)} %)
             cube-faces))))

;; Walking "around" the sides of the same cube.
(defn wrap-walk [cube-face walk-vec]
  (let [vec->side {[1 0 0] :right [-1 0 0] :left
                   [0 0 1] :top [0 0 -1] :bottom
                   [0 1 0] :back [0 -1 0] :front}]
    (assoc cube-face :side (vec->side walk-vec))))

;; Returns the new face (meaning cube coords and its side)
;; resulting from walking in the direction of walk-vec from cube-face.
(defn walk-vec->face [cube-face walk-vec]
  (if-some [blocking-face (blocking-walk cube-face walk-vec)]
    blocking-face
    (if-some [sidle-face (sidle-walk cube-face walk-vec)]
      sidle-face
      (wrap-walk cube-face walk-vec))))

(defn get-connected-faces [cube-face]
  (let [walk-vecs (side->walk-vecs (:side cube-face))
        next-faces (mapv #(walk-vec->face cube-face %) walk-vecs)]
    (into #{} next-faces)))

(defn walk-external-faces []
  (loop [visited-faces #{}
         current-faces #{starting-face}]
    (let [connected-faces
          (apply clojure.set/union (map get-connected-faces current-faces))
          new-faces
          (clojure.set/difference connected-faces visited-faces)]
      (if (empty? new-faces)
        visited-faces
        (do (pp (count visited-faces))
            (recur (clojure.set/union visited-faces current-faces)
                   new-faces))))))

(def external-faces (walk-external-faces)) ;; Slow!!!
(pp (count external-faces))
