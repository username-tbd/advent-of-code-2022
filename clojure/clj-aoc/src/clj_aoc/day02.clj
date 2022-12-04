(ns clj-aoc.day02
  (:require
   [clj-aoc.util :as u])
  (:gen-class))

(def lines
  (u/load-lines "../../inputs/input-02.txt"))

(def char->move {\A :rock \B :paper \C :scissors
                 \X :rock \Y :paper \Z :scissors})
(def shape-points {:rock 1 :paper 2 :scissors 3})

(defn outcome-points [opponent-move player-move]
  (cond
    (= opponent-move player-move) 3
    (some #{[opponent-move player-move]}
          [[:scissors :rock] [:rock :paper] [:paper :scissors]]) 6
    :else 0))

(defn line->moves [line]
  [(char->move (first line)) (char->move (last line))])

(defn round-points [[op pl]]
  (+ (outcome-points op pl) (shape-points pl)))

(->> lines
     (map (comp round-points line->moves))
     (apply +)
     println)

;; -----------
;; Part Two

(def char->meaning (assoc char->move \X :lose \Y :draw \Z :win))
(def response-map
  {:rock {:lose :scissors :draw :rock :win :paper}
   :paper {:lose :rock :draw :paper :win :scissors}
   :scissors {:lose :paper :draw :scissors :win :rock}})

(defn line->moves2 [line]
  (let [opponent-move (char->meaning (first line))
        desired-result (char->meaning (last line))]
    [opponent-move
     (get-in response-map [opponent-move desired-result])]))

(->> lines
     (map (comp round-points line->moves2))
     (apply +)
     println)
