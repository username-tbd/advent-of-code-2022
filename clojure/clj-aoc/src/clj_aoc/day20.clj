(ns clj-aoc.day20
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

;; Using a doubly-linked list. There might be a way to do it with a
;; singly-linked list but this just felt easier.
;; I'm using a vector to hold the nodes, with the vector indices
;; acting as the memory addresses.

(def numbers (mapv read-string (u/load-lines 20)))
(def n-numbers (count numbers))

(defn build-node [i n]
  {:value n
   :prev (mod (dec i) n-numbers)
   :next (mod (inc i) n-numbers)})

(def llist (vec (map-indexed build-node numbers)))

;; This function keeps node within the vector itself!
;; It just removes all pointers to it. We do this because we know
;; that the node will eventually be reinserted into the virtual llist structure
;; by altering pointers.
;; So we don't want to actually "remove" it in any real way.
(defn remove-node [llist node]
  (let [prev-address (:prev node)
        next-address (:next node)]
    (-> llist
        (assoc-in [prev-address :next] next-address)
        (assoc-in [next-address :prev] prev-address))))


;; Insert the node with vector address "address" after left-node.
;; We use address instead of node because we are never inserting
;; totally net-new nodes. We're always "inserting" a node which
;; already has a place in "memory" (i.e., the vector) but which
;; currently isn't pointed to by anyone else.
;; I tried using node instead of address, and the only way I could get
;; it to work was by continually conjing new nodes on the end of the vector.
;; I prefer them to stay put.
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

(def llist-mixed
  (reduce mix-nth llist (range (count llist))))

;; Find 0.
(defn find-in-llist [llist start-node value]
  (loop [node start-node]
    (if (= (:value node) value)
      node
      (let [next-node (get llist (:next node))]
        (if (= start-node next-node)
          nil
          (recur next-node))))))

;; We basically nulled out all the old vector addresses.
;; Just grab the first one we see
(def first-node (first (filter some? llist-mixed)))

(def zero-node
  (find-in-llist llist-mixed first-node 0))

(def coordinate-nodes
  (rest
    (reduce (fn [acc steps]
              (conj acc (walk-llist llist-mixed (peek acc) steps)))
            [zero-node]
            (repeat 3 1000))))

(pp (apply + (map :value coordinate-nodes)))
