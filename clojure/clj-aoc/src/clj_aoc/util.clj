(ns clj-aoc.util
  (:gen-class))

(defn load-lines [filepath]
  (with-open [rdr (clojure.java.io/reader filepath)]
    (doall (line-seq rdr))))
