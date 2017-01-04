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

(defn- neighbouring-walls
  [board point]
  (eduction
    (filter #(= :wall (board/get-cell-at board % :wall)))
    (utils/coord-neighbors point)))

(defn- compute-cell-strength
  "Compute a cell strength based on the number of walls it has"
  {:pre [(coord? point)]}
  [board point]
  (let [wall-nb (count (neighbouring-walls board point))]
    (+ 1 (* wall-nb wall-nb 0.25))))

(defn- compute-cells-strength
  "Adds to a given turn the strength of each of its cells"
  [board]
  (into {}
    (map (fn [point] [point (compute-cell-strength board point)]))
    cst/all-positions))

(defn- move-strength
  "Compute the strength of a move, based on the converted cells"
  [cells-strength move-filter [point converted]]
  {:pre [(fn? move-filter) (move/moves? converted)]}
  (transduce
    (comp
      (filter move-filter) ;; TODO - Extract this part (specific to worst move)
      (mapcat :taken)
      (map cells-strength))
    +
    (cells-strength point)
    converted))

(defn- worst-immediate-loss ;; TODO - It should consider what the other player could win
  "Return the next worse lost turn move for 'looser' if 'player' plays"
  {:pre [(player? player) (player? looser)]}
  [cells-strength turn player looser]
  (let [is-looser #(= looser (:looser %))
        all-moves (turn/get-moves-of turn player)]
    (transduce
      (map #(move-strength cells-strength is-looser %))
      max all-moves)))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn best-move
  "[SIMPLISTIC] Return the best move for a player based on:
   * The immediate gain
   * The worse immediate lost afterwards"
  {:pre [(player? player)]}
  [turn player]
  (let [cells-strength (compute-cells-strength (turn/get-board turn))
        moves (turn/get-moves-of turn player)
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
