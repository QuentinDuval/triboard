(ns triboard.ai.strategies
  (:require
    [triboard.ai.minimax :as minimax]
    ))


(defn score-sensible-ai
  [player]
  (reify minimax/AIStrategy
    (eval-score [_ scores] (get scores player))
    (maximizing? [_ turn] (= (:player turn) player))
    ))

(defn allied-against-player-ai
  []
  (reify minimax/AIStrategy
    (eval-score [_ scores] (+ (:red scores) (:green scores)))
    (maximizing? [_ turn] (not= (:player turn) :blue))
    ))
