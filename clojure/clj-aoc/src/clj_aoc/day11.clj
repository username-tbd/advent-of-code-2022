(ns clj-aoc.day11
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def monkey-maps-atom (atom []))

(def monkey-seqs
  (->> (u/load-lines 11)
       (partition-by empty?)
       (filter #(not= % (list "")))
       (map #(map clojure.string/trim %))))

(defn extract-starting-items [monkey-seq]
  (let [items-str (subs (nth monkey-seq 1) (count "Starting items: "))]
    (mapv read-string (clojure.string/split items-str #", "))))

(defn extract-operation-fn [monkey-seq]
  (let [[operator operand]
        (->
          (subs (nth monkey-seq 2) (count "Operation: new = old "))
          (clojure.string/split #" "))
        function
        (case operator "*" * "+" +)]
    (if (= operand "old")
      (fn [old] (function old old))
      (fn [old] (function old (read-string operand))))))

(defn extract-test-fn [monkey-seq]
  (let [lines
        (drop-while #(not (re-find #"Test" %)) monkey-seq)
        [div-num true-num false-num]
        (map (comp read-string #(re-find #"\d+" %)) lines)]
    (fn [worry]
      (if (zero? (mod worry div-num))
        true-num
        false-num))))

(defn build-monkey-map [monkey-seq]
  {:items (extract-starting-items monkey-seq)
   :operation-fn (extract-operation-fn monkey-seq)
   :test-fn (extract-test-fn monkey-seq)
   :inspected 0})

(def monkey-maps (mapv build-monkey-map monkey-seqs))

(defn throws-to [final-worry {:keys [test-fn]}]
    (test-fn final-worry))

(defn get-final-worry [worry {:keys [operation-fn]}]
  (let [post-inspection-worry (operation-fn worry)]
        ; (bigint (/ post-inspection-worry 3))))
        (bigint (/ post-inspection-worry 3))))

(defn debug-spit [monkey-maps]
  (spit "debug.txt" "\n-----------------\n" :append true)
  (loop [ms (map #(dissoc % :operation-fn :test-fn) monkey-maps)]
    (when (seq ms)
      (spit "debug.txt" (first ms) :append true)
      (spit "debug.txt" "\n" :append true)
      (recur (rest ms)))))

(defn debug-wipe []
  (spit "debug.txt" ""))

(defn debug-print [monkey-maps]
  (loop [ms (map #(dissoc % :operation-fn :test-fn) monkey-maps)]
    (when (seq ms)
      (println (first ms))
      (recur (rest ms)))))

(defn process-turn [monkey-maps monkey]
  (loop [monkey-maps monkey-maps]
    (if-some [item (get-in monkey-maps [monkey :items 0])]
      (let [monkey-map (nth monkey-maps monkey)
            final-worry (get-final-worry item monkey-map)
            throws-to (throws-to final-worry monkey-map)]
        (do
          (debug-spit monkey-maps)
          (reset! monkey-maps-atom monkey-maps)
          (recur
           (-> monkey-maps
               (update-in [monkey :items] #(vec (rest %)))
               (update-in [monkey :inspected] inc)
               (update-in [throws-to :items] #(conj % final-worry))))))
      monkey-maps)))

(defn process-round [monkey-maps]
  (loop [monkey-maps monkey-maps
         monkey 0]
    (if (= monkey (count monkey-maps))
      monkey-maps
      (recur (process-turn monkey-maps monkey) (inc monkey)))))


(debug-wipe)
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
