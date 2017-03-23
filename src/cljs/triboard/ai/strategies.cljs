(ns triboard.ai.strategies
  (:require
    [triboard.ai.minimax :as minimax]
    [triboard.logic.scores :as scores]
    [triboard.logic.transition :as transition]
    ))


;; TODO - For the AI sensible to the number of moves
;; We want to count the number of transitions available for the player

(defn- eval-leaf-score
  "Evaluate the score of a leaf turn by looking at its transition
   In effect, it will look the score one level after"
  [ai {:keys [scores] :as turn} eval-score]
  (minimax/minimax-step ai turn
    (fn [_ transition]
      (eval-score (scores/update-scores scores transition))
      )))

(defn score-sensible-ai
  [player]
  (reify minimax/AIStrategy
    (leaf-score [this turn] (eval-leaf-score this turn #(get % player)))
    (maximizing? [_ turn] (= (:player turn) player))
    ))

(defn allied-against-player-ai
  []
  (reify minimax/AIStrategy
    (leaf-score [this turn] (eval-leaf-score this turn #(+ (:red %) (:green %))))
    (maximizing? [_ turn] (not= (:player turn) :blue))
    ))

(defn like-freedom-ai
  [player]
  (reify minimax/AIStrategy
    (leaf-score [_ turn]
      (get (transition/all-transitions (:board turn)) player))
    (maximizing? [_ turn] (= (:player turn) player))
    ))
