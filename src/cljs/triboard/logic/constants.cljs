(ns triboard.logic.constants
  (:require
    [cljs.spec :as s :include-macros true]
    ))


(def board-width 16)
(def board-height 11)
(def init-block-count 12) ;; Init blocks for red, blue, green, gray
(def players [:blue :red :green])
(def directions [[-1 0] [1 0] [0 -1] [0 1] [-1 1] [1 1] [-1 -1] [1 -1]])
(def max-score (- (* board-width board-height) init-block-count))

(def all-positions
  (vec
    (for [x (range board-width)
          y (range board-height)]
      [x y])))

(def player? (set players))
(def cell? (conj player? :wall :empty))

(s/def ::player player?)
(s/def ::cell cell?)
