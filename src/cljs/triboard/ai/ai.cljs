(ns triboard.ai.ai
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.ai.minimax :as ai-algo]
    [triboard.ai.strategies :as strategies]
    [triboard.logic.board :as board]
    [triboard.logic.game :as game]
    [triboard.logic.turn :as turn]))


;; TODO - Plug other ways to score (like counting available transitions)
;; * The high level AI will select the available transitions in early game
;; * The high level AI will select the score for the late game


;; -----------------------------------------
;; Private
;; -----------------------------------------

;; TODO - Most of this can be moved in minimax since we extracted the leaf-score into interface

(defn- tree-score
  "Implements the minimax recursion:
   * Call the leaf node evaluation if the depth is zero
   * Otherwise goes one level deeper"
  [ai turn depth]
  (if (zero? depth)
    (ai-algo/leaf-score ai turn)
    (ai-algo/minimax-step ai turn
      (fn [_ transition]
        (tree-score ai
          (turn/next-turn turn transition)
          (dec depth))))
    ))

(defn- best-move
  "The top level of the minimax algorithm
   * Triggers lower level minimax evaluations
   * Keeps the transition that led to the max"
  [ai turn]
  (first
    (ai-algo/minimax-step-by
      second ai turn
      (fn [coord transition]
        (let [new-turn (turn/next-turn turn transition)]
          [coord (tree-score ai new-turn 1)]))
      )))

(defn- focus-human-player?
  "High level AI to decude whether the AI ally against the human player"
  [{:keys [blue red green] :as scores}]
  (let [human-score (* 1.1 blue)]
    (and (< red human-score) (< green human-score))))

(defn- high-level-ai
  [turn]
  (cond
    (focus-human-player? (:scores turn)) (strategies/allied-against-player-ai)
    :else (strategies/score-sensible-ai (:player turn))
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
        ai (high-level-ai turn)
        coord (time (best-move ai turn))]  ;; TODO - Remove time + find a way to correlate with moves + sort then and take best
    coord))


;; -----------------------------------------
;; TESTS
;; -----------------------------------------

(defn benchmark []
  (let [g (game/new-game)]
    (time (dotimes [i 10]
            (time (find-best-move g))
            ))))
