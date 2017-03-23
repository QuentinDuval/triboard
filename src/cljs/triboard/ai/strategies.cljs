(ns triboard.ai.strategies
  (:require
    [triboard.ai.minimax :as minimax]
    ))


;; TODO - For the AI sensible to the number of moves
;; We do not care about transitions or new scores
;; This is one level lower and we want to keep the
;; available moves for each player
;; This means we want something different for leaf score

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
