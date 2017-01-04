(ns triboard.ai.ai
  (:require
    [triboard.ai.scores :as scores]
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
  [get-cells-strength player [point converted]]
  {:pre [(move/conversions? converted)]}
  (reduce
    #(scores/update-delta get-cells-strength %1 %2)
    scores/null-delta
    (conj converted
      (move/empty-cell-conversion player point))
    ))

(defn- worst-immediate-loss ;; TODO - It should consider what the other player could win
  "Return the next worse lost turn move for 'looser' if 'player' plays"
  {:pre [(player? player) (player? looser)]}
  [cells-strength turn player looser]
  (let [all-moves (turn/get-moves-of turn player)]
    (transduce
      (map #(get (move-strength cells-strength player %) looser))
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
                diff-score (get (move-strength cells-strength player move) player)
                losses (map #(worst-immediate-loss cells-strength new-turn % player) others)]
            (- diff-score (apply max losses))))
        moves))
    ))
