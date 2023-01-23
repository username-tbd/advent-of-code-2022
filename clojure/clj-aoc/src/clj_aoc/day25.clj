(ns clj-aoc.day25
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}]
            [clojure.math :as math])
  (:gen-class))

(def snafu-char->dec-char {\= -2 \- -1 \0 0 \1 1 \2 2})
(def dec-char->snafu-char (zipmap (vals snafu-char->dec-char)
                                   (keys snafu-char->dec-char)))

;; 1=11-2 (i.e., 2022) becomes [1 -2 1 1 -1 2]
(def snafus (mapv #(mapv snafu-char->dec-char %) (u/load-lines 25)))

(defn add-helper [x y z]
  ((fnil + 0 0 0) x y z))

;; Figured out what this should look like manually.
(defn place-helper [place-sum-decimal]
  ({0 0, 1 1, 2 2, 3 -2, 4 -1} (mod place-sum-decimal 5)))

;; Same (figured out the pattern) but with even less understanding.
(defn carry-helper [place-sum-decimal]
  (if (pos? place-sum-decimal)
    (quot (+ place-sum-decimal 2) 5)
    (quot (- place-sum-decimal 2) 5)))

(defn add-snafus [x y]
  (loop [x x y y
         sum-snafu-rev []
         carry 0]
    (if (and (empty? x)
             (empty? y)
             (zero? carry))
      (vec (reverse sum-snafu-rev))
      (let [place-sum-decimal (add-helper (last x) (last y) carry)
            place (place-helper place-sum-decimal)
            new-carry (carry-helper place-sum-decimal)]
        (recur (butlast x) (butlast y)
               (conj sum-snafu-rev place)
               new-carry)))))

(def snafu-sum (reduce add-snafus snafus))
(apply str (map dec-char->snafu-char snafu-sum))

;; Might be needed for part 2
(defn snafu->decimal [x]
  (let [exponents (reverse (range (count x)))
        powers (map #(math/pow 5 %) exponents)
        place-values (map * powers x)]
    (reduce + place-values)))

