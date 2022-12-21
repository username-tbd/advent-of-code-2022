(ns clj-aoc.day21
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

(def lines (u/load-lines 21))

(defn yeller [line]
  (keyword (subs line 0 4)))

(defn line->map [line]
  (if-some [number-str (re-find #"\d+" line)]
    {(yeller line)
     {:number (read-string number-str)}}
    {(yeller line)
     {:operation (load-string (subs line 11 12))
      :operands [(keyword (subs line 6 10))
                 (keyword (subs line 13))]}}))

(def monkey-map (apply merge (map line->map lines)))

(defn find-numbers [monkey-map [monkey-1 monkey-2]]
  (if-some [n1 (:number (monkey-1 monkey-map))]
    (if-some [n2 (:number (monkey-2 monkey-map))]
      [n1 n2])))

(defn resolve-operation [monkey-map monkey]
  (let [operands (:operands (monkey monkey-map))
        operation (:operation (monkey monkey-map))]
    (if-some [numeric-operands (find-numbers monkey-map operands)]
      (apply operation numeric-operands))))

(defn process-monkey [monkey-map monkey]
  (if (some? (:number (monkey monkey-map)))
    monkey-map
    (if-some [resolved-operation (resolve-operation monkey-map monkey)]
      (assoc-in monkey-map [monkey :number] resolved-operation)
      monkey-map)))

(defn single-pass [monkey-map]
  (let [monkeys (keys monkey-map)]
    (loop [ret monkey-map
           n-processed 0]
      (if (= (count monkey-map) n-processed)
        ret
        (recur (process-monkey ret (nth monkeys n-processed))
               (inc n-processed))))))

(defn pass-until-complete [monkey-map]
  (loop [ret monkey-map]
    (let [one-pass (single-pass ret)]
      (if (= ret one-pass)
       ret
      (recur one-pass)))))

(def monkey-map-final (pass-until-complete monkey-map))
(pp (:root monkey-map-final))
