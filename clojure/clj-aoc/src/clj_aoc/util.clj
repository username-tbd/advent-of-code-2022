(ns clj-aoc.util
  (:gen-class))

(defn load-lines [day]
  (with-open [rdr (clojure.java.io/reader
                    (format "../../inputs/input-%02d.txt" day))]
    (doall (line-seq rdr))))
