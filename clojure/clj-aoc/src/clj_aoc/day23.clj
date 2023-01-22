(ns clj-aoc.day23
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

;; Started with vecs of vecs in part 1, but I'm changing this to
;; maps of maps for part 2 so we can expand the structure out freely.
;; We'll start them out with one layer of extra padding on all four sides.
;; After that, we'll start getting negative indices
;; (which is why I'm using maps here).
;; We will grow the whole structure by one in all four directions every round.

(defn vec->map [v]
  (zipmap (range (count v)) v))

(def lines (u/load-lines 23))
(def grove-init-vecs (mapv #(mapv {\. :empty \# :elf} %) lines))
(def grove-init
  (update-vals (vec->map grove-init-vecs) vec->map))

(defn pad-row [row minimum-key maximum-key]
    (-> row
        (assoc minimum-key :empty)
        (assoc maximum-key :empty)))

(defn pad-all-sides [grove]
  (let [new-min-key (dec (apply min (keys grove)))
        new-max-key (inc (apply max (keys grove)))
        grove-rows-padded (update-vals grove
                                       #(pad-row % new-min-key new-max-key))
        top-bottom-pad-vals (repeat (inc (- new-max-key new-min-key)) :empty)
        top-bottom-pad (zipmap (range new-min-key (inc new-max-key))
                               top-bottom-pad-vals)]
    (merge grove-rows-padded
           {new-min-key top-bottom-pad}
           {new-max-key top-bottom-pad})))

(def grove-init-padded (pad-all-sides grove-init))

(def cardinals-cycle (cycle [:n :s :w :e]))

(def direction->steps
  {:n [-1 0] :s [1 0] :w [0 -1] :e [0 1]
   :ne [-1 1] :nw [-1 -1] :se [1 1] :sw [1 -1]})

(def cardinal->directions
  {:n #{:nw :n :ne}
   :s #{:sw :s :se}
   :w #{:sw :w :nw}
   :e #{:se :e :ne}})

(defn get-adjacent-cells [grove elf-index]
  (let [adjacent-inds
        (update-vals direction->steps #(mapv + % elf-index))]
    (update-vals adjacent-inds #(get-in grove %))))

(defn isolated? [adjacent-cells]
  (not-any? #(= :elf %) (vals adjacent-cells)))

(defn cardinal-free-of-elves? [adjacent-cells cardinal]
  (let [directions (cardinal->directions cardinal)
        relevant-cells (filter #(contains? directions (key %)) adjacent-cells)]
    (not-any? #(= :elf %) (vals relevant-cells))))

(defn get-proposition-index [elf-index cardinal]
  (mapv + elf-index (direction->steps cardinal)))

(defn prop-from-cardinal [elf-index adjacent-cells cardinal]
  (let [prop-ind (get-proposition-index elf-index cardinal)]
    (if (cardinal-free-of-elves? adjacent-cells cardinal)
      prop-ind)))

;; Get proposition index for an elf and an ordered seq of cardinals.
;; If no proposition, returns nil.
(defn elf-proposition [grove elf-index cardinals]
  (let [adjacent-cells (get-adjacent-cells grove elf-index)]
    (if-not (isolated? adjacent-cells)
      (->>
        (mapv (partial prop-from-cardinal elf-index adjacent-cells)
              cardinals)
        (filter some?)
        first))))

(defn get-elf-indices [grove]
  (let [grove-keys (distinct (keys grove))]
    (for [row grove-keys
          col grove-keys
          :when (= :elf (get-in grove [row col]))]
      [row col])))

;; Remove any propositions that appear more than once
(defn clean-proposition-map [proposition-map]
  (let [freqs (frequencies (vals proposition-map))
        allowed-inds (keys (filter #(= 1 (val %)) freqs))]
    (into {} (filter #(some #{(val %)} allowed-inds) proposition-map))))

;; Map from vector to vector
(defn gen-proposition-map [grove cardinals]
  (let [elf-indices (get-elf-indices grove)
        map-with-nils (zipmap elf-indices
                              (map #(elf-proposition grove % cardinals)
                                   elf-indices))
        proposition-map (into {} (filter #(some? (val %)) map-with-nils))]
    (clean-proposition-map proposition-map)))

(defn make-move [grove prop]
  (-> grove
      (assoc-in (key prop) :empty)
      (assoc-in (val prop) :elf)))

(defn make-moves [grove proposition-map]
  (loop [grove grove
         props proposition-map] 
    (if (empty? props)
      grove
      (recur (make-move grove (first props))
             (rest props)))))

;; Pad all sides after we're done making moves!
(defn process-round [grove cardinals]
  (let [proposition-map (gen-proposition-map grove cardinals)]
    (if (empty? proposition-map) ; for part 2
      (/ 5 0)
      (->>
        (make-moves grove proposition-map)
        (pad-all-sides)))))
    
(defn process-rounds [grove cardinals-cycle n-rounds]
  (loop [grove grove
         cardinals-cycle cardinals-cycle
         n-rounds-left n-rounds ; pt 1 
         round 1] ; pt 2 
    (spit "hi.txt" round :append true)
    (spit "hi.txt" "\n" :append true)
    (if (zero? n-rounds-left)
      grove
      (recur (process-round grove (take 4 cardinals-cycle))
             (rest cardinals-cycle)
             (dec n-rounds-left)
             (inc round)))))

(def grove-final (process-rounds grove-init-padded cardinals-cycle 10))

(defn get-subrect-cells [grove]
  (let [elf-inds (get-elf-indices grove)
        elf-rows (map first elf-inds)
        elf-cols (map second elf-inds)
        grove-keys (distinct (keys grove))]
    (for [row grove-keys
          col grove-keys
          :when (and (<= (apply min elf-rows) row (apply max elf-rows))
                     (<= (apply min elf-cols) col (apply max elf-cols)))]
      (get-in grove [row col]))))

(->> grove-final
     get-subrect-cells
     (filter #(= :empty %))
     count)

;; ----- Part 2

(process-rounds grove-init-padded cardinals-cycle 10000000)


