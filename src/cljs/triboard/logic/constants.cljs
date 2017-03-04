(ns triboard.logic.constants)

(def init-block-count 12) ;; Init blocks for red, blue, green, gray
(def directions [[-1 0] [1 0] [0 -1] [0 1] [-1 1] [1 1] [-1 -1] [1 -1]])
#_(def max-score (- (* board-width board-height) init-block-count))
