(ns clj-aoc.day11
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

;; Very ugly

(def monkey-seqs
  (->> (u/load-lines 11)
       (partition-by empty?)
       (filter #(not= % (list "")))
       (map #(map clojure.string/trim %))))

(defn extract-starting-items [monkey-seq]
  (let [items-str (subs (nth monkey-seq 1) (count "Starting items: "))]
    (mapv read-string (clojure.string/split items-str #", "))))

;; Represent as map instead of actual function for now.
(defn extract-operation [monkey-seq]
  (let [[operator operand]
        (->
          (subs (nth monkey-seq 2) (count "Operation: new = old "))
          (clojure.string/split #" "))]
    (if (= operand "old")
      {:operator operator :operand :old}
      {:operator operator :operand (read-string operand)})))

(defn extract-divisor [monkey-seq]
  (let [line
        (nth monkey-seq 3)]
    ((comp read-string #(re-find #"\d+" %)) line)))

(defn extract-recipients [monkey-seq]
  (let [lines
        (drop-while #(not (re-find #"If true" %)) monkey-seq)]
    (->>
      (mapv (comp read-string #(re-find #"\d+" %)) lines)
      (zipmap [:true :false]))))

(defn build-monkey-map [monkey-seq]
  {:items (extract-starting-items monkey-seq)
   :operation (extract-operation monkey-seq)
   :divisor (extract-divisor monkey-seq)
   :recipients (extract-recipients monkey-seq)
   :inspected 0})

(def monkey-maps (mapv build-monkey-map monkey-seqs))

(defn throws-to [final-worry {:keys [divisor recipients]}]
  (if (zero? (mod final-worry divisor))
    (:true recipients)
    (:false recipients)))

(defn apply-operation [worry {:keys [operator operand]}]
  (if (= operator "+")
    (+ operand worry)
    (if (= operand :old)
      (* worry worry)
      (* worry operand))))
  
(defn get-final-worry [worry {:keys [operation]}]
  (quot (apply-operation worry operation) 3))

(defn process-turn [monkey-maps monkey]
  (loop [monkey-maps monkey-maps]
    (if-some [item (get-in monkey-maps [monkey :items 0])]
      (let [monkey-map (nth monkey-maps monkey)
            final-worry (get-final-worry item monkey-map)
            throws-to (throws-to final-worry monkey-map)]
        (recur
          (-> monkey-maps
              (update-in [monkey :items] #(vec (rest %)))
              (update-in [monkey :inspected] inc)
              (update-in [throws-to :items] #(conj % final-worry)))))
      monkey-maps)))

(defn process-round [monkey-maps]
  (loop [monkey-maps monkey-maps
         monkey 0]
    (if (= monkey (count monkey-maps))
      monkey-maps
      (recur (process-turn monkey-maps monkey) (inc monkey)))))

(def monkey-maps-final
  (loop [monkey-maps monkey-maps
         round 0]
    (if (= round 20)
      monkey-maps
      (recur (process-round monkey-maps) (inc round)))))

(->> (map :inspected monkey-maps-final)
     (sort >)
     (take 2)
     (apply *)
     println)

;; ----------
;; Part two

(def divisors (mapv :divisor monkey-maps))

(defn build-item-mods [item]
  (zipmap divisors (mapv (partial mod item) divisors)))

(defn items->item-mods-vec [items]
  (mapv build-item-mods items))

(defn assoc-item-mods-vec [monkey-map]
  (assoc monkey-map
         :item-mods-vec
         (items->item-mods-vec (:items monkey-map))))

(def monkey-maps-2
  (mapv assoc-item-mods-vec monkey-maps))

(defn apply-operation-mod [curr-mod divisor {:keys [operator operand]}]
  (if (= operator "+")
    (mod (+ operand curr-mod) divisor)
    (if (= operand :old)
      (mod (* curr-mod curr-mod) divisor)
      (mod (* operand curr-mod) divisor))))

(defn operate-item-mods [item-mods {:keys [operation]}]
  (let [update-mods
        (fn [item-mods divisor]
          (update item-mods divisor apply-operation-mod divisor operation))]
    (reduce update-mods item-mods divisors)))
    
(defn process-turn-2 [monkey-maps monkey]
  (loop [monkey-maps monkey-maps]
    (if-some [item-mods (get-in monkey-maps [monkey :item-mods-vec 0])]
      (let [monkey-map (nth monkey-maps monkey)
            new-item-mods (operate-item-mods item-mods monkey-map)
            monkey-divisor (monkey-map :divisor)
            throws-to (throws-to (new-item-mods monkey-divisor) monkey-map)]
        (recur
          (-> monkey-maps
              (update-in [monkey :item-mods-vec] #(vec (rest %)))
              (update-in [monkey :inspected] inc)
              (update-in [throws-to :item-mods-vec] #(conj % new-item-mods)))))
      monkey-maps)))

(defn process-round-2 [monkey-maps]
  (loop [monkey-maps monkey-maps
         monkey 0]
    (if (= monkey (count monkey-maps))
      monkey-maps
      (recur (process-turn-2 monkey-maps monkey) (inc monkey)))))

(def monkey-maps-final-2
  (loop [monkey-maps monkey-maps-2
         round 0]
    (if (= round 10000)
      monkey-maps
      (recur (process-round-2 monkey-maps) (inc round)))))

(->> (map :inspected monkey-maps-final-2)
     (sort >)
     (take 2)
     (apply *)
     println)
