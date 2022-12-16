(ns clj-aoc.day11
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

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

;;-----------------------------------------------------------

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
