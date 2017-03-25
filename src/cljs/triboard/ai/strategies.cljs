(ns triboard.ai.strategies
  (:require
    [triboard.ai.minimax :as minimax]
    [triboard.logic.scores :as scores]
    [triboard.logic.transition :as transition]
    ))


;; TODO - For the AI sensible to the number of moves
;; We want to count the number of transitions available for the player

(defn- eval-next-score-of
  "Evaluate the score of a leaf turn by looking at its transition
   In effect, it will look the score one level after"
  [ai {:keys [scores] :as turn} players]
  (minimax/minimax-step ai turn
    (fn [_ transition]
      (let [scores (scores/update-scores scores transition)]
        (apply + (map #(get scores %) players)))
      )))

(defn optimize-own-score-ai
  [player]
  (reify minimax/AIStrategy
    (eval-turn [this turn] (eval-next-score-of this turn [player]))
    (maximizing? [_ turn] (= (:player turn) player))
    ))

(defn optmize-ai-scores-ai
  []
  (reify minimax/AIStrategy
    (eval-turn [this turn] (eval-next-score-of this turn [:red :green]))
    (maximizing? [_ turn] (not= (:player turn) :blue))
    ))

(defn optimize-own-choices-ai
  [player]
  (reify minimax/AIStrategy
    (eval-turn [_ turn]
      (count (get (transition/all-transitions (:board turn)) player)))
    (maximizing? [_ turn] (= (:player turn) player))
    ))
