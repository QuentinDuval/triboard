(ns triboard.ai.strategies
  (:require
    [triboard.ai.minimax :as minimax]
    [triboard.logic.scores :as scores]
    [triboard.logic.transition :as transition]
    ))


;; TODO - For the AI sensible to the number of moves
;; We want to count the number of transitions available for the player

(defn- eval-next-score-of
  "Perform a last minimax step on the next scores following the turn
   * Allows to see one level deeper for simple scoring strategies
   * While being fast (the transition does not need to be followed)"
  [ai {:keys [scores] :as turn} players]
  (minimax/minimax-step ai turn
    (fn [_ transition]
      (let [scores (scores/update-scores scores transition)]
        (apply + (map #(get scores %) players)))
      )))

(defn optimize-own-score-ai
  "Create an AI strategy to optmize its own score: good late game play"
  [player]
  (reify minimax/AIStrategy
    (eval-turn [this turn] (eval-next-score-of this turn [player]))
    (maximizing? [_ turn] (= (:player turn) player))
    ))

(defn optmize-ai-scores-ai
  "Create an AI strategy to optmize the AI scores: good cheat when the player wins"
  []
  (reify minimax/AIStrategy
    (eval-turn [this turn] (eval-next-score-of this turn [:red :green]))
    (maximizing? [_ turn] (not= (:player turn) :blue))
    ))

(defn optimize-own-choices-ai
  "Create an AI strategy to optmize the AI choices: avoid being trapped with no moves left"
  [player]
  (reify minimax/AIStrategy
    (eval-turn [_ turn]
      (count (get (transition/all-transitions (:board turn)) player)))
    (maximizing? [_ turn] (= (:player turn) player))
    ))
