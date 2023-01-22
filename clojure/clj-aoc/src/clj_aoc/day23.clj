(ns clj-aoc.day23
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))

;; For expanding the board, I'm just going to use the hacky solution
;; of padding by 11 in every direction. With ten rounds that should be enough.

(defn pad-line [line]
  (str "..........." line "..........."))

(def lines (u/load-lines 23))
(def lines-padded-rows (map pad-line lines))
(def padded-line-count (count (first lines-padded-rows)))
(def full-pad-line
  (apply str (repeat padded-line-count  ".")))

(def lines-padded (concat (repeat 11 full-pad-line)
                          lines-padded-rows
                          (repeat 11 full-pad-line)))


(def grove-init (mapv #(mapv {\. :empty \# :elf} %) lines-padded))
(def cardinals-cycle (cycle [:n :s :w :e]))
(def n-row (count grove-init))
(def n-col (count (first grove-init)))

(defn in-bounds? [index]
  (and
    (<= 0 (first index) (dec n-row))
    (<= 0 (second index) (dec n-col))))

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
    (if (and (cardinal-free-of-elves? adjacent-cells cardinal)
             (in-bounds? prop-ind))
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
  (for [row (range n-row)
        col (range n-col)
        :when (= :elf (get-in grove [row col]))]
    [row col]))

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

(defn process-round [grove cardinals]
  (let [proposition-map (gen-proposition-map grove cardinals)]
    (make-moves grove proposition-map)))
    
(defn process-rounds [grove cardinals-cycle n-rounds]
  (loop [grove grove
         cardinals-cycle cardinals-cycle
         n-rounds-left n-rounds]
    (if (zero? n-rounds-left)
      grove
      (recur (process-round grove (take 4 cardinals-cycle))
             (rest cardinals-cycle)
             (dec n-rounds-left)))))

(def grove-final (process-rounds grove-init cardinals-cycle 10))

(defn get-subrect-cells [grove]
  (let [elf-inds (get-elf-indices grove)
        elf-rows (map first elf-inds)
        elf-cols (map second elf-inds)]
    (for [row (range n-row)
          col (range n-col)
          :when (and (<= (apply min elf-rows) row (apply max elf-rows))
                     (<= (apply min elf-cols) col (apply max elf-cols)))]
      (get-in grove [row col]))))

(->> grove-final
     get-subrect-cells
     (filter #(= :empty %))
     count)
