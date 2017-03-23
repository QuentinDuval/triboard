(ns triboard.ai.minimax
  (:require [triboard.logic.turn :as turn]))


(defprotocol AIStrategy
  (eval-score [this scores] "Heuristic to score the value of a turn")
  (maximizing? [this turn] "Tell whether we are in min or max step"))


(defn minimax-step
  "One stage of the minimax algorithm:
   * Apply the maximizing or mininizing step to all transitions of the turn
   * Evaluate the lower level using the on-transition function"
  [ai turn on-transition
   & {:keys [max-fn min-fn]
      :or {max-fn max, min-fn min}}]
  (apply
    (if (maximizing? ai turn) max-fn min-fn)
    (map
      (fn [[coord transition]] (on-transition coord transition))
      (turn/transitions turn))))

(defn minimax-step-by
  [key-fn ai turn on-transition]
  (minimax-step
    ai turn on-transition
    :min-fn (partial min-key key-fn) ;; Lambda does not work here
    :max-fn (partial max-key key-fn)))
