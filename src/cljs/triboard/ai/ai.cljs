(ns triboard.ai.ai
  (:require
    [triboard.ai.scores :as scores]
    [triboard.logic.constants :as cst]
    [triboard.logic.move :as move]
    [triboard.logic.turn :as turn]
    [triboard.utils :as utils]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- score-move
  "Compute the strength of a move, based on the converted cells"
  [weights-by-cell player [point conversions]]
  {:pre [(move/conversions? conversions)]}
  (reduce
    #(scores/update-score-diff weights-by-cell %1 %2)
    scores/null-score-diff
    (conj conversions
      (move/empty-cell-conversion player point))
    ))

(defn- worst-immediate-loss ;; TODO - It should consider what the other player could win
  "Return the next worse lost turn move for 'looser' if 'player' plays"
  {:pre [(player? player) (player? looser)]}
  [weights-by-cell turn player looser]
  (let [all-moves (turn/get-moves-of turn player)]
    (scores/min-delta-for looser
      (map #(score-move weights-by-cell player %) all-moves)
      )))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn best-move
  "[SIMPLISTIC] Return the best move for a player based on:
   * The immediate gain
   * The worse immediate lost afterwards"
  {:pre [(player? player)]}
  [turn player]
  (let [weights-by-cell (scores/get-weights-by-cell (turn/get-board turn))
        others (remove #{player} cst/players)]
    (first
      (utils/fast-max-key
        (fn [[m converted :as move]]
          (let [new-turn (turn/play-move turn m)
                new-diff (get (score-move weights-by-cell player move) player)
                next-diff (map #(worst-immediate-loss weights-by-cell new-turn % player) others)]
            (+ new-diff (apply min next-diff))))
        (turn/get-moves-of turn player)
        ))
    ))


