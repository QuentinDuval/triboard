(ns triboard.ai.ai
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.ai.minimax :as ai-algo]
    [triboard.ai.strategies :as strategies]
    [triboard.logic.board :as board]
    [triboard.logic.game :as game]
    [triboard.logic.turn :as turn]))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- move-tree-search
  "The top level of the minimax algorithm
   * Trigger sub-game-trees minimax evaluations
   * Remember the transition that led to the max"
  [ai turn]
  (first
    (ai-algo/minimax-step-by
      second ai turn
      (fn [coord transition]
        (let [new-turn (turn/next-turn turn transition)]
          [coord (ai-algo/minimax ai new-turn 1)]))
      )))

(defn- human-player-winning?
  [{:keys [blue red green] :as scores}]
  (let [human-score (* 1.1 blue)]
    (and (< red human-score) (< green human-score))))

(defn- limited-move-options?
  [turn]
  (< (count (turn/transitions turn)) 5))

(defn- in-late-game?
  [scores]
  (let [max-score (count board/coordinates)
        sum-score (apply + (vals scores))]
    (< (- max-score sum-score) 20)))

(defn- high-level-ai
  "High level AI: choose the right evaluation function to play"
  [{:keys [player scores] :as turn}]
  (cond
    (human-player-winning? scores) (strategies/optmize-ai-scores-ai)
    (in-late-game? scores) (strategies/optimize-own-score-ai player)
    (limited-move-options? turn) (strategies/optimize-own-choices-ai player)
    :else (strategies/optimize-own-score-ai player)
    ))

;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef find-best-move
  :args (s/cat :game ::game/game)
  :ret ::board/coord)

(defn find-best-move
  "Find the best available move for the current player"
  [game]
  (let [turn (game/current-turn game)
        ai (high-level-ai turn)]
    (move-tree-search ai turn)))
