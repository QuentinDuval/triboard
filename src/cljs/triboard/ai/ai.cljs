(ns triboard.ai.ai
  (:require
    [triboard.ai.scores :as scores]
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    [triboard.logic.move :as move]
    [triboard.logic.turn :as turn]
    [triboard.utils :as utils]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- to-ai-input
  [turn player]
  {:player player
   :other-players (remove #{player} cst/players)
   :cell-weights (scores/get-weights-by-cell (turn/get-board turn))
   })

(defn- score-move
  "Compute the strength of a move, based on the converted cells"
  [{:keys [player cell-weights]} [point conversions]]
  {:pre [(move/conversions? conversions)]}
  (reduce
    #(scores/update-score-diff cell-weights %1 %2)
    scores/null-score-diff
    (conj conversions
      (move/empty-cell-conversion player point))
    ))

(defn- next-worst-possible-score
  [{:keys [player other-players] :as ai-input} turn]
  (scores/min-delta-for player
    (eduction
      (comp
        (mapcat #(turn/get-moves-of turn %))
        (map #(score-move ai-input %)))
      other-players)))

(defn- move-best-outcome
  [ai-input turn [coord converted :as move]]
  (let [new-turn (turn/play-move turn coord)
        move-diff (get (score-move ai-input move) (:player ai-input))
        next-diff (next-worst-possible-score ai-input new-turn)]
    (+ move-diff next-diff)))


;; -----------------------------------------
;; Public API
;; -----------------------------------------
(defn best-move
  "[SIMPLISTIC] Return the best move for a player based on:
   * The immediate gain
   * The worse immediate lost afterwards"
  {:pre [(player? player)] :post [(board/coord? %)]}
  [turn player]
  (let [ai-input (to-ai-input turn player)]
    (first
      (utils/fast-max-key #(move-best-outcome ai-input turn %) (turn/get-moves-of turn player))
      )))
