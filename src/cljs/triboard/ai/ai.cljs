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

(defn- get-worst-possible-score-for
  [weights-by-cell turn player other-players]
  (let [all-adverse-moves (mapcat #(turn/get-moves-of turn %) other-players)
        all-possible-scores (map #(score-move weights-by-cell player %) all-adverse-moves)]
    (scores/min-delta-for player all-possible-scores)))

(defn- move-best-outcome
  [weights-by-cell player others turn [coord converted :as move]]
  (let [new-turn (turn/play-move turn coord)
        move-diff (get (score-move weights-by-cell player move) player)
        next-diff (get-worst-possible-score-for weights-by-cell new-turn player others)]
    (+ move-diff next-diff)))


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
        #(move-best-outcome weights-by-cell player others turn %)
        (turn/get-moves-of turn player)
        ))
    ))

#_(
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
       :other-players (remove #{player cst/players})
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

    (defn- get-worst-possible-score-for
      [{:keys [player other-players] :as ai-input} turn]
      (let [all-adverse-moves (mapcat #(turn/get-moves-of turn %) other-players)
            all-possible-scores (map #(score-move ai-input %) all-adverse-moves)]
        (scores/min-delta-for player all-possible-scores)
        ))

    (defn- move-best-outcome
      [ai-input turn [coord converted :as move]]
      (let [new-turn (turn/play-move turn coord)
            move-diff (get (score-move ai-input move) (:player ai-input))
            next-diff (get-worst-possible-score-for ai-input new-turn)]
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

    )

