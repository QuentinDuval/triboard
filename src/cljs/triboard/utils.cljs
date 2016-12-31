(ns triboard.utils
  (:require
    [triboard.logic.constants :as cst]
    ))

(defn fast-max-key
  "Fast max key that avoids recomputing things several times"
  {:pre [(fn? key-fn) (seq? coll)]}
  [key-fn coll]
  (apply max-key (memoize key-fn) coll))

(defn coord-neighbors
  "All neighbors of a given coordinate"
  [[x y]]
  (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) cst/directions))
