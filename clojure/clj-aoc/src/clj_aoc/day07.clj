(ns clj-aoc.day07
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def lines (u/load-lines 7))

(def line-maps
  (map #(into {} [[:cd (last (re-find #"^\$ cd (\S+)" %))]
                  [:ls (some? (re-find #"^\$ ls$" %))]
                  [:filesize (if-some [fsize (last (re-find #"^(\d+)" %))]
                               (read-string fsize))]])
       lines))

(defn build-size-map [line-maps]
  (loop [line-maps line-maps
         size-map {}
         current-dirs []]
    (let [line (first line-maps)
          update-size-map (fn [sm dir] (update sm dir + (:filesize line)))]
      (cond
        (nil? line)
        size-map
        (= (:cd line) "..")
        (recur (rest line-maps)
               size-map
               (pop current-dirs))
        (:cd line)
        (let [dir-path (str (last current-dirs) (:cd line) "/")]
          (recur (rest line-maps)
                 (conj size-map [dir-path 0])
                 (conj current-dirs dir-path)))
        (:filesize line)
        (recur (rest line-maps)
               (reduce update-size-map size-map current-dirs)
               current-dirs)
        :else
        (recur (rest line-maps)
               size-map
               current-dirs)))))

(def size-map (build-size-map line-maps))

(->> size-map
     (filter #(< (second %) 100000))
     vals
     (apply +))

;; -----------
;; Part Two

(def unused-space (- 70000000 (size-map "//")))
(def need-to-free (- 30000000 unused-space))

(->> size-map
     (filter #(> (second %) need-to-free))
     (sort #(- (second %1) (second %2)))
     first)
