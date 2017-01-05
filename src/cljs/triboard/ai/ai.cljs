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

(defn- make-ai
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
    (conj conversions (move/empty-cell-conversion player point))
    ))

(defn- worst-possible-score-from
  [{:keys [player other-players] :as ai} turn]
  (transduce
    (comp
      (mapcat #(turn/get-moves-of turn %))
      (map #(score-move ai %))
      (map #(get % player)))
    min
    other-players))

(defn- move-best-outcome
  [ai turn [coord converted :as move]]
  (let [new-turn (turn/play-move turn coord)
        move-diff (get (score-move ai move) (:player ai))
        next-diff (worst-possible-score-from ai new-turn)]
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
  (let [ai (make-ai turn player)]
    (first
      (utils/max-by #(move-best-outcome ai turn %) (turn/get-moves-of turn player))
      )))
