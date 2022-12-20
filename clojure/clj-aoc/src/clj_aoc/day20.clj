(ns clj-aoc.day20
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

;; Using a circular doubly-linked list.
;; I'm using a vector to hold the nodes, with the vector indices
;; acting like memory addresses.

(def numbers (mapv read-string (u/load-lines 20)))
(def n-numbers (count numbers))

(defn build-node [i n]
  {:value n
   :prev (mod (dec i) n-numbers)
   :next (mod (inc i) n-numbers)})

(def llist (vec (map-indexed build-node numbers)))

;; This function keeps node within the vector itself!
;; It just removes the two pointers to it. 
;; We know that the node will soon be reinserted into the
;; virtual llist structure by updating pointers, so we don't want
;; to actually "remove" it in any real way.
(defn remove-node [llist node]
  (let [prev-address (:prev node)
        next-address (:next node)]
    (-> llist
        (assoc-in [prev-address :next] next-address)
        (assoc-in [next-address :prev] prev-address))))

;; Insert the node with vector index "address" after left-node.
(defn insert-after [llist address left-node]
  (let [right-address (:next left-node)
        left-address (:prev (get llist right-address))]
    (-> llist
        (assoc-in [address :prev] left-address)
        (assoc-in [address :next] right-address)
        (assoc-in [left-address :next] address)
        (assoc-in [right-address :prev] address))))

(defn walk-llist [llist start-node n]
  (loop [node start-node
         n n]
    (if (zero? n)
      node
      (if (pos? n)
        (recur (nth llist (:next node)) (dec n))
        (recur (nth llist (:prev node)) (inc n))))))

(defn mix-nth [llist n]
  (let [node (get llist n)
        llist-without-node (remove-node llist node)
        left-node (walk-llist llist-without-node
                              (get llist-without-node (:prev node))
                              (:value node))]
    (insert-after llist-without-node n left-node)))

(def vec-inds (range n-numbers))
(def llist-mixed
  (reduce mix-nth llist vec-inds))

;; Will need to find 0.
(defn find-in-llist [llist value]
  (let [start-node (get llist 0)]
    (loop [node start-node]
      (if (= (:value node) value)
        node
        (let [next-node (get llist (:next node))]
          (if (not= start-node next-node) 
            (recur next-node)))))))

(defn get-coordinate-nodes [llist coordinate-inds]
  (let [zero-node (find-in-llist llist  0)
        step-numbers (mapv - coordinate-inds (concat [0] coordinate-inds))]
    (rest
      (reduce (fn [acc steps]
                (conj acc (walk-llist llist (peek acc) steps)))
              [zero-node]
              step-numbers))))

(def coordinate-nodes
  (get-coordinate-nodes llist-mixed [1000 2000 3000]))

(pp (apply + (map :value coordinate-nodes)))

;; -------------
;; Part two
(def n-mixes 10)
(defn decrypt [value]
  (* value 811589153))

;; Decrypted values are very large - we need to find the remainders.
;; Calling this compression to go along with the problem's "decryption".
;; Decrement because we walk the llist WITHOUT the current node!
(defn compress [value]
  (rem value (dec n-numbers)))

;; Replace :value with the decrypted and compressed value.
;; We add in :value-uncompressed because we will need that for the answer.
(def llist-pt2
  (->> llist 
       (mapv #(assoc % :value-uncompressed (decrypt (:value %))))
       (mapv #(update % :value (comp compress decrypt)))))

(def llist-mixed-pt2
  (reduce mix-nth llist-pt2
          (take (* n-mixes n-numbers) (cycle vec-inds))))

(def coordinate-nodes-pt2
  (get-coordinate-nodes llist-mixed-pt2 [1000 2000 3000]))

(pp (apply + (map :value-uncompressed coordinate-nodes-pt2)))

