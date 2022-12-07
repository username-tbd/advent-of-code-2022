(ns clj-aoc.day07
  (:require [clj-aoc.util :as u])
  (:gen-class))

(def lines (u/load-lines 7))

(def line-maps
  (map #(into {} [[:cd (last (re-find #"^\$ cd (\S+)" %))]
                  [:ls (some? (re-find #"^\$ ls$" %))]
                  [:filesize (if-some [string (last (re-find #"^(\d+)" %))]
                               (read-string (last (re-find #"^(\d+)" %)))
                               nil)]])
       lines))

(defn build-size-map [line-maps]
  (loop [line-maps line-maps
         size-map {}
         current-dirs []]
    (let [line (first line-maps)]
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
        (let [update-size-map
              (fn [sm dir] (update sm dir #(+ % (:filesize line))))]
          (recur (rest line-maps)
                 (reduce update-size-map size-map current-dirs)
                 current-dirs))
        :else
        (recur (rest line-maps)
               size-map
               current-dirs)))))

(->> (build-size-map line-maps)
     (filter #(< (second %) 100000))
     vals
     (apply +))
