(ns triboard.logic.constants)


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

