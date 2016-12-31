(ns triboard.logic.board
  (:require
    [triboard.logic.constants :as cst]
    ))


(defn board?
  "A board is a vector of vector of cells"
  [b]
  (every? #(every? cst/cell? %) b))
