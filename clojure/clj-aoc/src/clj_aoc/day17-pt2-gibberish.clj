;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; This file contains everything I went through to get part 2, which was a lot.
;; Keeping this for posterity instead of cleaning it up.
;; Starting at "++++++++" is where I really figured it out.
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; Current status - made it farther in the loop.
;; Now @nfc is not relevant
;; The problem is in jet-piece or drop-piece.

;;;;; I think the bigger problem is that the jets don't really
;; line up with the shapes... I tried to reduce over the shapes
;; to keep using process-piece. But that's impossible!
;; Getting the same number of shapes as n-jets doesn't work because
;; you don't get one jet per shape. It's many jets per shape.

;;;;;; Possible solution:
;; The jets and the shapes line up every 5th full jet cycle.
;; (starting on the 4th or 5th or something.)
;; So, record how many times you hit one of those eclipses and then
;; note how many of those there have been whenever you get a full-7 floor.
;; Then you can figure out how many of those there are until the trillionth
;; and then you know.

(ns clj-aoc.day17
  (:require [clj-aoc.util :as u]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
  (:gen-class))
(def n-free-calls (atom 0))
(def occupancies-atom (atom 0))
(def xy-atom (atom 0))
(def nfc (atom 168559))



;; cycle is very cool. Used twice here.

(def jets-infinite
  (->> (first (u/load-lines 17))
    (map (comp keyword str))
    cycle))

(def n-iters 2022)
(def x-margin 2)
(def y-margin 4)
(def jet-to-shift {:< [-1 0] :> [1 0]})
(defn jet-to-shift-debug [jet]
  (if (= @n-free-calls @nfc) (println "jet-to-shift arg:"))
  (if (= @n-free-calls @nfc) (println jet))
  (jet-to-shift jet))



;; 0th row is floor, all spaces occupied
(def occupancies-initial [(into #{} (range 7))]) 

(defn shift-inds [shift inds]
  (mapv #(mapv + shift %) inds))

(def piece-blueprints
  (map (partial shift-inds [x-margin y-margin])
       [[[0 0] [1 0] [2 0] [3 0]]
        [[1 0] [0 1] [1 1] [2 1] [1 2]]
        [[0 0] [1 0] [2 0] [2 1] [2 2]]
        [[0 0] [0 1] [0 2] [0 3]]
        [[0 0] [0 1] [1 0] [1 1]]]))

(def piece-blueprints-2022
  (take n-iters (cycle piece-blueprints)))

;; get returns nil if key is not found, and (not-any? #{x} nil) returns true!
(defn free? [occupancies [x y]]
  (swap! n-free-calls inc)
  ;; Putting a do here doesn't work. Can't nest?
  (if (= @n-free-calls @nfc) (println "xxxxxxxxx"))
  (if (= @n-free-calls @nfc) (reset! occupancies-atom occupancies))
  (if (= @n-free-calls @nfc) (reset! xy-atom [x y]))
  (if (= @n-free-calls @nfc) (pp @n-free-calls))
  (if (= @n-free-calls @nfc) (pp (take-last 20 occupancies)))
  (if (= @n-free-calls @nfc) (pp [x y]))
  (if (= @n-free-calls @nfc) (pp (<= 0 x 6)))
  (if (= @n-free-calls @nfc) (pp (< 0 y)))
  (if (= @n-free-calls @nfc) (pp (get occupancies y)))
  (if (= @n-free-calls @nfc) (pp (not-any? #{x} (get occupancies y))))
  (if (= @n-free-calls @nfc) (println "oooooooooo"))
  (and (<= 0 x 6) ;; Not hitting a wall
       (< 0 y)
       (not-any? #{x} (get occupancies y))))

(defn jet-piece [piece-inds occupancies jet]
  (let [piece-post-jet (shift-inds (jet-to-shift-debug jet) piece-inds)]
    (if (= @n-free-calls @nfc) (println "-----"))
    (if (= @n-free-calls @nfc) (println (count @occupancies-atom)))
    (if (= @n-free-calls @nfc) (println "piece-inds:"))
    (if (= @n-free-calls @nfc) (println piece-inds))
    (if (= @n-free-calls @nfc) (println "piece-post-jet:"))
    (if (= @n-free-calls @nfc) (println piece-post-jet))
    (if (every? (partial free? occupancies) piece-post-jet)
      piece-post-jet
      piece-inds)))

;; Return nil if the piece cannot move any more
(defn drop-piece [piece-inds occupancies]
  (if (= @n-free-calls @nfc) (println "in drop-piece: " @n-free-calls))
  (let [piece-post-drop (shift-inds [0 -1] piece-inds)]
    (if (every? (partial free? occupancies) piece-post-drop)
      piece-post-drop
      nil)))

;; (update [0 10 20] 3 f) => [0 10 20 (f nil)]
;; (clojure.set/union nil #{1}) => #{1}
(defn update-occupancies [occupancies piece]
  (reduce #(update %1 (second %2) clojure.set/union #{(first %2)})
          occupancies piece))

;; Returns map of updated occupancies vector and jets
(defn process-piece [{:keys [occupancies jets]} piece-blueprint]
  (let [highest-row (dec (count occupancies))
        piece-init (shift-inds [0 highest-row] piece-blueprint)]
    (loop [piece-inds piece-init
           jets jets] 
      (let [piece-jetted
            (jet-piece piece-inds occupancies (first jets))]
        (if-some [piece-dropped (drop-piece piece-jetted occupancies)]
          (recur piece-dropped (rest jets))
          {:occupancies (update-occupancies occupancies piece-jetted)
           :jets (rest jets)})))))

(def initial-state {:occupancies occupancies-initial
                    :jets jets-infinite})

(def final-state
  (reduce process-piece initial-state piece-blueprints-2022))

(pp (dec (count (:occupancies final-state))))

;; ----------
;; Part two

;; Can we find a case where:
; 1. We have run through exactly all 10,091 jets some number of times
; 2. The top row is just like the floor (7 positions all filled)

;; Jets is now finite, pieces are now infinite
;; Update: seems like I'll just use length of piece-blueprints
;; to end the looping. So won't really use this other than the count).

(def jets-finite ;; i.e., not an infinite, lazy-seq cycle.
  (->> (first (u/load-lines 17))
    (map (comp keyword str))))
(def n-jets (count jets-finite))
(def piece-blueprints-infinite
  (cycle piece-blueprints))

(defn state-after-all-jets [occupancies piece-blueprints]
  (reduce process-piece {:occupancies occupancies
                         :jets jets-infinite}
          piece-blueprints))

; ;; The key is that piece-blueprints is the length of all jets.
; ;; I reduced over piece-blueprints before, and this lets me 
; ;; continue to do that. Couldn't figure out how to refactor otherwise.
; (defn find-cycle-num [] ;; Using globals...; is this cool?
;   (loop [cycle-num 1
;          occupancies occupancies-initial
;          [current-blueprints rest-blueprints]
;          (split-at n-jets piece-blueprints-infinite)]
;     (let [new-state (state-after-all-jets occupancies current-blueprints)
;           new-occupancies (:occupancies new-state)]
;       (if (= (count (last new-occupancies)) 7)
;         (inc cycle-num)
;         (recur (inc cycle-num)
;                new-occupancies
;                rest-blueprints)))))
; (def hi (find-cycle-num))

;; -----------
(pp (take 5 (:occupancies final-state)))

(def meta-cycle-length
  (* (count piece-blueprints) (count jets-finite)))

; (defn do-n-meta-cycles [n]
;   (let [meta-cycle-length
;         (* (count piece-blueprints) (count jets-finite))
;         n-pieces-in-all-cycles
;         (* n meta-cycle-length)
;         n-pieces-with-beginning
;         (+ 1 n-pieces-in-all-cycles)
;         piece-blueprints-meta
;         (take n-pieces-with-beginning (cycle piece-blueprints))]
;     (reduce process-piece initial-state piece-blueprints-meta)))

;; Values of interest:
;; After a meta cycle AND the following flat piece, we care about
;; the last two occupancies. These are our values of interest.
;; In particular, we want them to be functionally the same for two
;; different numbers of metacycles.
;; This means that the last occupancy (in which the flat piece resides)
;; is the same, while the set-difference of the last occupancy and
;; the penultimate occupancy is the same.

;; To make it easy, I'm going to see if we can find any where
;; this matches up with the beginning - meta-cycle 0...

;; Figured the following out after writing out the positions of the first
;; five pieces that fall with my input data.
;; In order for us to get a repeat after some number of meta-cycles,
;; we need two things out of the final occupancies row (which is 
;; analogous to the floor in beginning):
;; 1. One of 0-3 needs to be occupied. This is to ensure that the
;; initial horizontal piece lands in its same place (0123).
;; 2. 5 needs to be occupied. This is where the vertical piece lands.
(defn works-like-floor? [occupancies-row]
  (boolean (and (some #{0 1 2 3} occupancies-row)
                (some #{5} occupancies-row))))

(def piece-blueprints-len-of-meta
  (take meta-cycle-length (cycle piece-blueprints)))

(def first-10-jets-after (atom []))

(defn get-last-meta-cycle-rows [n-meta-cycles]
  (loop [meta-cycles-completed 0
         state initial-state
         last-rows []]
    (let [state-after-meta-cycle
          (reduce process-piece state piece-blueprints-len-of-meta)
          occ-after-meta-cycle (:occupancies state-after-meta-cycle)]
      (if (= (inc meta-cycles-completed) n-meta-cycles)
        (conj last-rows (last occ-after-meta-cycle))
        (do (swap! first-10-jets-after conj
                   (take 20 (:jets state-after-meta-cycle)))
          (recur (inc meta-cycles-completed)
                state-after-meta-cycle
                ; {:occupancies occ-after-meta-cycle
                ;  :jets jets-infinite}
                (conj last-rows (last occ-after-meta-cycle))))))))

;; This shows us that the first meta cycle ends with #{2 3 4}
;; and after that they always end in #{6}.
(def last-meta-cycle-rows (get-last-meta-cycle-rows 5)) 
(pp last-meta-cycle-rows)

;; Now that we know it settles pretty quickly, let's tweak that 
;; code to give us the height after n meta cycles so we can derive
;; the function by hand.
(defn get-heights-after-metas [n-meta-cycles]
  (loop [meta-cycles-completed 0
         state initial-state
         heights []]
    (let [state-after-meta-cycle
          (reduce process-piece state piece-blueprints-len-of-meta)
          occ-after-meta-cycle (:occupancies state-after-meta-cycle)
          height-after-meta-cycle (dec (count occ-after-meta-cycle))]
      (if (= (inc meta-cycles-completed) n-meta-cycles)
        (conj heights height-after-meta-cycle)
        (recur (inc meta-cycles-completed)
               state-after-meta-cycle
               ; {:occupancies occ-after-meta-cycle
               ;  :jets jets-infinite}
               (conj heights height-after-meta-cycle))))))

(def heights (get-heights-after-metas 5)) 
(pp heights)
(pp (mapv - (rest heights) heights)) ;; Look at the diffs

;; After looking at that, we know that after the first meta cycle,
;; we settle into the following function:
;; height = 78422 * [n-meta-cycles] + 4
;; Calculation doesn't work until 2
(defn calculate-height [n-meta-cycles]
  (if (= 1 n-meta-cycles)
    (first heights)
    (+ (* 78422 n-meta-cycles) 4)))

;; So now, how many total meta-cycles would we do before we hit the
;; big part 2 number?
(def n-iters-pt-2
  1000000000000)

(def n-meta-cycles-pt-2
  (quot n-iters-pt-2 meta-cycle-length))
(pp n-meta-cycles-pt-2) ;; => 19819641

(def n-leftover-pieces-pt-2
  (mod n-iters-pt-2 meta-cycle-length))
(pp n-leftover-pieces-pt-2) ; => 13345

(def height-after-meta-cycles (calculate-height n-meta-cycles-pt-2))
(pp height-after-meta-cycles) ;; => 1554295886506

;; Now we need to simulate the leftover drops.
;; Tricky part: [explain this - we need two meta cycles]
(def piece-blueprints-two-cycles-and-leftover
  (take
    (+ (* 2 meta-cycle-length) n-leftover-pieces-pt-2)
    (cycle piece-blueprints)))

(def two-cycles-and-leftover-occupancies
  (:occupancies
    (reduce process-piece initial-state
            piece-blueprints-two-cycles-and-leftover)))

(def leftover-height (- (dec (count two-cycles-and-leftover-occupancies))
                        (calculate-height 2)))

(def total-height-pt-2
  (+ height-after-meta-cycles
     leftover-height))
(pp total-height-pt-2)

;; 1554295907263 too high
;; 1554295907262 too high
;; 1554295907246 too high


;; -------------------------------------------------------
;; Any cases where we get both pieces and jets restarting at the
;; same time? Update: looks like no.
(def jets-start-identifier
  (map (comp keyword str) "><<<<>><<<<>>>><>>><<<"))

(def n-jets-start (count jets-start-identifier))
(def fifth-piece (last piece-blueprints))

;; Replacing the reduce
(defn sup []
  (loop [n-pieces-completed 0
         state initial-state
         pieces piece-blueprints-infinite]
    (let [curr-piece (first pieces)
          state-after-process (process-piece state curr-piece)
          next-jets-start (take n-jets-start (:jets state-after-process))]
      (do
        ; (pp curr-piece)
        (if (and (= jets-start-identifier next-jets-start)
                 true)
                 ; (= curr-piece fifth-piece))
          {:n-pieces-completed (inc n-pieces-completed)
           :final-state state-after-process
           :curr-piece curr-piece}
          (recur (inc n-pieces-completed)
                 state-after-process
                 (rest pieces)))))))

; (def supsup (sup))

;; ------------------------------------------------
;; ++++++++
;; See if we can find a time where we hit a combo we've seen before, of:
;; same piece
;; AND the same last 5 of occupancies rows (5 since our tallest piece is 4)
;; AND we are at the same point in jets

(defn lol []
  (loop [n-pieces-completed 0
         state initial-state
         pieces piece-blueprints-infinite
         combos #{}]
    (let [curr-piece (first pieces)
          state-after-process (process-piece state curr-piece)
          occ-after-process (:occupancies state-after-process)
          last-5-occs (take-last 5 occ-after-process)
          combo {:piece curr-piece
                 :last-5 last-5-occs
                 :jets (take n-jets (:jets state-after-process))}]
      (if (some #{combo} combos)
        {:n-pieces-completed (inc n-pieces-completed)
         :final-occupancies (:occupancies state-after-process)
         :curr-piece curr-piece
         :final-combo combo}
        (recur (inc n-pieces-completed)
               state-after-process
               (rest pieces)
               (conj combos combo))))))

(def loll (lol))
(pp (select-keys loll [:n-pieces-completed :curr-piece]))


;; So we found how many pieces until we hit that second one.
;; Let's get how long it takes to hit it, and how many
;; are in between.
(def relevant-combo
  (:final-combo loll))

(defn oh []
  (loop [n-pieces-completed 0
         n-relevant-combo-hits 0
         n-pieces-completed-at-relevant-combo-hits []
         height-at-relevant-combo-hits []
         state initial-state
         pieces piece-blueprints-infinite]
    (let [curr-piece (first pieces)
          state-after-process (process-piece state curr-piece)
          occ-after-process (:occupancies state-after-process)
          last-5-occs (take-last 5 occ-after-process)
          combo {:piece curr-piece
                 :last-5 last-5-occs
                 :jets (take n-jets (:jets state-after-process))}
          new-n
          (if (= combo relevant-combo)
            (inc n-relevant-combo-hits)
            n-relevant-combo-hits)
          new-n-pieces-vec
          (if (= combo relevant-combo)
            (conj n-pieces-completed-at-relevant-combo-hits
                  (inc n-pieces-completed))
            n-pieces-completed-at-relevant-combo-hits)
          new-height-vec
          (if (= combo relevant-combo)
            (conj height-at-relevant-combo-hits
                  (dec (count occ-after-process)))
            height-at-relevant-combo-hits)]
      (if (= 5 new-n)
        {:n-pieces-completed (inc n-pieces-completed)
         :n-pieces-completed-at-relevant-combo-hits
         n-pieces-completed-at-relevant-combo-hits
         :height-at-relevant-combo-hits
         height-at-relevant-combo-hits
         :final-occupancies (:occupancies state-after-process)
         :final-jets (:jets state-after-process)
         :final-combo combo}
        (recur (inc n-pieces-completed)
               new-n
               new-n-pieces-vec
               new-height-vec
               state-after-process
               (rest pieces))))))

(def ohh (oh))
(def relevant-n-pieces (:n-pieces-completed-at-relevant-combo-hits ohh))
(pp relevant-n-pieces) ;; => (74 1774 3474 5174)
(pp (mapv - (rest relevant-n-pieces)
          relevant-n-pieces)) ;; => 1700 lag always!!!

(def heights (:height-at-relevant-combo-hits ohh))
(pp heights)
(pp (mapv - (rest heights) heights))

;; So: for every piece an integer multiple of 1700 in excess of 74,
;; height is (2642/1700) * piece-num + 3823/425.
(defn get-height-from-cycle [cycle-num]
  (let [piece-num (+ 74 (* 1700 cycle-num))]
    (+ (* (/ 2642 1700) piece-num) (/ 3823 425))))

; How many cycles do we have in between 74 and 1 trillion?
;; THIS MATH MIGHT BE WRONG
(def n-cycles (quot (- n-iters-pt-2 74) 1700))
(def n-pieces-after-cycles (mod (- n-iters-pt-2 74) 1700))

(def height-after-n-cycles
  (get-height-from-cycle n-cycles))

(def initial-state-after-cycles
  {:occupancies (:final-occupancies ohh)
   :jets (:final-jets ohh)})

(def subtract-me-off
  (count (:final-occupancies ohh)))

;; drop 4 since the cycle uses the fourth piece
(def piece-blueprints-excess
  (take n-pieces-after-cycles (drop 4 (cycle piece-blueprints))))

(def excess-height
  (- (count (:occupancies
              (reduce process-piece initial-state-after-cycles
                      piece-blueprints-excess)))
     subtract-me-off))

(pp (+ height-after-n-cycles excess-height))
