(ns triboard.ai.ai
  (:require
    [triboard.logic.constants :as cst]
    [triboard.logic.board :as board]
    [triboard.logic.move :as move]
    [triboard.logic.turn :as turn]
    [triboard.utils :as utils]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- compute-cell-strength
  "Compute a cell strength based on the number of walls it has"
  {:pre [(coord? point)]}
  [board point]
  (let [neighbors (utils/coord-neighbors point)
        walls (filter #(= :wall (board/get-cell-at board % :wall)) neighbors)
        count-wall (count walls)]
    (+ 1 (* count-wall count-wall 0.25))))

(defn- move-strength
  "Compute the strength of a move, based on the converted cells"
  [cells-strength converted-filter [point converted]]
  {:pre [(fn? converted-filter) (every? move/move? converted)]}
  (transduce
    (comp
      (filter converted-filter) ;; TODO - Extract this part (specific to worst move)
      (mapcat :taken)
      (map cells-strength))
    +
    (cells-strength point)
    converted))

(defn- worst-immediate-loss ;; TODO - It should consider what the other player could win
  "Return the next worse lost turn move for 'looser' if 'player' plays"
  {:pre [(player? player) (player? looser)]}
  [cells-strength turn player looser]
  (let [converted-filter #(= looser (:looser %))
        all-moves (get-in turn [:moves player])]
    (transduce
      (map #(move-strength cells-strength converted-filter %))
      max all-moves)))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn compute-cells-strength
  "Adds to a given turn the strength of each of its cells"
  [board]
  (reduce
    #(assoc %1 %2 (compute-cell-strength board %2))
    {} cst/all-positions))

(defn best-move
  "[SIMPLISTIC] Return the best move for a player based on:
   * The immediate gain
   * The worse immediate lost afterwards"
  {:pre [(player? player)]}
  [cells-strength turn player]
  (let [moves (get-in turn [:moves player])
        others (remove #{player} cst/players)]
    (first
      (utils/fast-max-key
        (fn [[m converted :as move]]
          (let [new-turn (turn/play-move turn m)
                diff-score (move-strength cells-strength identity move)
                losses (map #(worst-immediate-loss cells-strength new-turn % player) others)]
            (- diff-score (apply max losses))))
        moves))
    ))
